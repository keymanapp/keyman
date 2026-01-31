export interface RecognitionZoneSource {
  /**
   * Returns the zone's bounding rectangle in .clientX/.clientY coordinates,
   * matching the behavior of HTMLElement.getBoundingClientRect().
   */
    getBoundingClientRect(): DOMRect;
}