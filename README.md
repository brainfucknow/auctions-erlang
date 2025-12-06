# Auctions Erlang

An Erlang-based REST API server for managing auctions with support for multiple auction types (English, Blind, and Vickrey auctions).

## Prerequisites

- [Erlang/OTP (v27)](https://www.erlang.org/)
- [rebar3](https://rebar3.org/)

## Building

```bash
rebar3 compile
```

## Running

```bash
bin/start_server
```

The server will start on `http://localhost:8080`.

## Running Tests

```bash
rebar3 eunit
```

## Project Structure

- `src/` - Application source code
  - `auctions_app.erl` - Application entry point
  - `auctions_router.erl` - HTTP route definitions
  - `*_handler.erl` - HTTP request handlers
  - `*_auction.erl` - Auction type implementations
  - `auction_store.erl` - Persistence layer
  - `auction_serialization.erl` - JSON serialization

- `test/` - Test modules

- `priv/` - Application data (auctions.json)


