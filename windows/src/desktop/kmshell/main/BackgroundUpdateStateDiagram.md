``` mermaid
stateDiagram
    [*] --> Idle
    Idle --> UpdateAvailable
    UpdateAvailable --> Downloading
    Downloading --> Installing
    Downloading --> WaitingRestart
    WaitingRestart --> Installing
    Installing --> Idle
```
