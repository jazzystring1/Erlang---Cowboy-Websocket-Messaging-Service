# Erlang Cowboy Websocket Messaging Service REST API

An Erlang OTP application as a websocket using Cowboy for sending messages in full-duplex. This application handles the state of every user, the presence of every users and perfectly scalable

## Requirements
	Rebar3 as build tool
	
## Deploying to Heroku

You can refer to this tutorial https://github.com/madcat78/cowboy-websocket-example by madcat78. Special thanks to `madcat78` for the Erlang Heroku webpack.

## API Usage


### Authentication
	// Host
	https://YOUR_DOMAIN.com/

	// Endpoint
	POST - /apiv1/connect

	// headers
	Authorization: Basic Y2xpZW50aWQ6c2VjcmV0
	Accept: application/json  
	Content-Type: application/json

	// request body
	grant_type=client_credentials 

### Sending Message

## Build and Run

	$ rebar3 compile
	$ rebar3 shell
		

	
