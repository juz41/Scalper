# poker-scala

A Scala implementation of 5-Card Draw Poker built on Play Framework, designed for incremental expansion.

## Running

### Console game
```bash
sbt "runMain engine.ConsoleGame"
```

### Play web server
```bash
sbt run
# then open http://localhost:9000
```

### Tests
```bash
sbt test
```

---

## Module breakdown

```
app/
├── model/          Domain models – pure data, no side effects
│   ├── Card.scala      Card, Rank, Suit and their companion objects
│   ├── Deck.scala      Immutable deck with shuffle / deal operations
│   └── Player.scala    Player, Hand, PlayerType
│
├── logic/          Pure business rules – no I/O, fully unit-testable
│   ├── HandRank.scala      Sealed hierarchy of all 10 poker hand rankings
│   └── HandEvaluator.scala Hand evaluation + showdown winner resolution
│
├── engine/         Game orchestration layer
│   ├── Action.scala        ADT for player actions (Fold / Call / Raise)
│   ├── ComputerAI.scala    Random decision-making for the computer player
│   └── ConsoleGame.scala   Phase 1 entry point: full console game loop
│
└── controllers/    [Stub] Play Framework HTTP layer – Phase 2
    └── HomeController.scala

test/
├── model/
│   ├── CardSpec.scala      Card, Rank, Suit unit tests
│   └── DeckSpec.scala      Deck shuffle / deal unit tests
└── logic/
    └── HandEvaluatorSpec.scala  All hand ranks + tiebreaker + winner tests
```

---

## Planned phases

| Phase | Scope |
|-------|-------|
| **1 – Console** *(current)* | Core models, hand evaluator, random AI, console game loop, unit tests |
| **2 – REST API** | Play Framework JSON controllers; game sessions via `app/services/`; HTTP routes for start/action/state |
| **3 – Pekko actors** | `app/actors/` – one actor per game session for concurrent multiplayer |
| **4 – Persistence** | `app/repositories/` – player registry, chip balances, game history |
| **5 – Frontend** | JavaScript client or Twirl views consuming the REST API |

---

## Design notes

- **Immutable data everywhere** – `Player`, `Deck`, `Hand` are case classes; every action returns a new value. This makes the game loop easy to follow, test, and later map to actor messages.
- **Pure logic layer** – `HandEvaluator` has no side effects and is independently testable without starting Play.
- **Pluggable AI** – `ComputerAI` is a plain object; replacing random moves with strategy-based logic requires no changes outside that file.
- **Comments explain *why*, not *what*** – as requested, code comments focus on the rationale for decisions rather than restating the code.
