# Scalper

A Scala implementation of Poker built on Play Framework.  
This project aims to create a web app that allows users to play poker.

---

## Module breakdown

- **Game engine** – core domain models (`Card`, `Deck`, `Hand`, `Player`), hand evaluator, and game loop logic
- **Desktop app** – Play Framework routes and controllers; Client consuming the REST API

---

## Division of labor

| Area | Owner |
|------|-------|
| Game engine & hand evaluator | Patryk Ząbik |
| REST API & Play controllers | Paweł Szymański |
| AI opponents | Julian Zalewski |
| Frontend / views | Julian Zalewski |
| Tests & CI | Paweł Szymański, Patryk Ząbik |

---


## Possible additional features

- different poker variations
- account, leaderboard, social features
- game history features (best hand, statistics)
- "true" (as in neural network) AI

---
