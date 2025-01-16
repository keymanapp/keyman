``` mermaid
stateDiagram
    [*] --> Idle
    Idle --> UpdateAvailable
    UpdateAvailable --> Downloading
    Downloading --> Installing
    Downloading --> WaitingRestart
    WaitingRestart --> Installing
    Installing --> PostInstall
    PostInstall --> Idle
```
State Transition  matrix
