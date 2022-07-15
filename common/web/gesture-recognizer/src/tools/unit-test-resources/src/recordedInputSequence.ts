namespace Testing {
  type InputSequence = com.keyman.osk.InputSequence;

  export interface RecordedInputSequence {
    sequence: InputSequence;
    terminationEvent?: "end" | "cancel"
  }
}