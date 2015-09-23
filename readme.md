### shelduck

shelduck is a hybrid web-server/api-client. Its main use is as an opinionated tool for QAing webhooks on remote services.

shelduck is made up of a few concurrent components:

* An [ngrok](https://ngrok.com/) client is used to forward a local service. shelduck expects ngrok in your path, and a fixed ngrok url (requires a Pro account) with a configuration block like:

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

Check out src/IntercomDefinitions.hs for some descriptions of [Intercom webhooks](https://doc.intercom.io/api/#webhooks-and-notifications).

You can run shelduck by concurrently starting the server and request engine:

```haskell
run :: TVar TopicResult -> IO ()
run t = void $ do
  options <- opts
  go $ blank & requestEndpoint .~ "https://api.intercom.io/contacts"
             & requestOpts .~ options
             & requestParameters .~ object []
             & requestTopic .~ "contact.created"
  -- ...
  where go = runDefinition t


runDefinition :: TVar TopicResult -> WebhookRequest -> IO (W.Response L.ByteString)
runDefinition t w = do
  x <- runReaderT req t
  return $ x ^. response
  where req = performRequest w

runDefinitions :: IO ()
runDefinitions = do
  info "Running definitions"
  r <- newTVarIO Nothing :: IO (TVar TopicResult)
  concurrently (server r) (run r)
  return ()
```

### Keen support

Test runs are automatically sent to [Keen](https://keen.io/) if `KEEN_PROJECT_ID` and `KEEN_API_KEY` are set in your Env.

### Slack support

Test failures are sent to Slack as a webhook if `SLACK_WEBHOOK_URL` is set in your Env.
