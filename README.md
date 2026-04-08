# poker-scala

A Scala implementation of Poker built on Play Framework.  
This project aims to create a web app that allows users to play poker.

---

## Module breakdown

- **Game engine** – core domain models (`Card`, `Deck`, `Hand`, `Player`), hand evaluator, and game loop logic
- **Frontend and web app** – Play Framework routes and controllers; Twirl views or a JavaScript client consuming the REST API
- **AI for playing poker** – pluggable computer opponents, starting with random moves and expanding toward strategy-based logic

---

## Division of labor

| Area | Owner |
|------|-------|
| Game engine & hand evaluator | Patryk Ząbik |
| REST API & Play controllers | Paweł Szymański |
| AI opponents | Julian Zalewski, Patryk Ząbik |
| Frontend / views | Julian Zalewski |
| Tests & CI | Paweł Szymański |

---


## Possible additional features

- different poker variations
- account, leaderboard, social features
- game history features (best hand, statistics)
- "true" (as in neural network) AI

---
