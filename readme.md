### shelduck

[![Build
Status](https://semaphoreci.com/api/v1/projects/dff9e854-4c40-49ac-afc9-204f9a70c370/585391/badge.svg)](https://semaphoreci.com/robertjflong/shelduck)

shelduck is a hybrid web-server/api-client. Its main use is as an opinionated tool for QAing webhooks on remote services. Read about it [here](http://www.boblong.co/automated-webhook-qa/).

shelduck is made up of a few concurrent components:

* An [ngrok](https://ngrok.com/) client is used to forward a local service. shelduck expects ngrok in your path, and a fixed ngrok url (requires a paid account) with a configuration block like:

```
# ~/.ngrok2/ngrok.yml
authtoken: foobarbaz
tunnels:
  shelduck:
    hostname: "yoururl.grok.io"
    proto: http
    addr: 8080
```

This fixed ngrok URL is what you should use to create subscriptions in the service you are testing.

* A web-service is run to catch and record incoming webhooks.

* An API client/data-types/DSL is used to describe what topics result from what API actions. For example the following asserts that creating a user via the [Intercom](https://www.intercom.io) API fires a webhook with topic `user.created`:

```haskell
blank & requestEndpoint .~ "https://api.intercom.io/users"
      & requestOpts .~ options
      & requestParameters .~ object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
      & requestTopic .~ "user.created"
```

* A templater is used to splice in different attributes. For example {{random}} injects a UUID.

### Running shelduck

Check out `src/Shelduck/IntercomDefinitions.hs` for some descriptions of [Intercom webhooks](https://doc.intercom.io/api/#webhooks-and-notifications).

You can run shelduck by concurrently starting the server and request engine:

```haskell
run :: TVar TopicResult -> StateT DefinitionListRun IO ()
run t = void $ do
  -- ...
  go $ blank & requestEndpoint .~ "https://api.intercom.io/users"
             & requestOpts .~ options
             & requestParameters .~ object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
             & requestTopic .~ "user.created"
  -- ...
  where go :: WebhookRequest -> StateT DefinitionListRun IO (W.Response L.ByteString)
        go = ((^. response) <$>) . runAssertion t

runIntercomDefinitions :: IO ()
runIntercomDefinitions = do
  info "Running Intercom definitions"
  r <- newTVarIO Nothing :: IO (TVar TopicResult)
  withAsync (server r) $ \webServer ->
    withAsync (runDefs r) $ \testRun -> wait testRun >> cancel webServer
  return ()
  where runDefs r = execStateT (run r) defaultDefinitionListRun
```

### Web UI

Currently, shelduck writes to `~/shelduck.log` (this will be configurable one day). A web-ui for this log file is made available at `localhost:4567/web-ui`.

<img src="https://s3-eu-west-1.amazonaws.com/bobblogimages/Screen+Shot+2015-10-25+at+20.19.03.png"/>

### Keen support

Test runs are automatically sent to [Keen](https://keen.io/) if `KEEN_PROJECT_ID` and `KEEN_API_KEY` are set in your Env.

### Slack support

Test failures are sent to Slack as a webhook if `SLACK_WEBHOOK_URL` is set in your Env.
