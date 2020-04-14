namespace com.keyman.text.prediction {
  export interface WorkerFactory {
    constructInstance(): Worker;
  }
}