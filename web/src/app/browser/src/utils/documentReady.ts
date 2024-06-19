export function whenDocumentReady(): Promise<void> {
  if(document.readyState === 'complete') {
    return Promise.resolve();
  }

  return new Promise((resolve, reject) => {
    const loadHandler: (e: Event) => void = () => {
      window.removeEventListener('load', loadHandler);

      resolve();
    };

    window.addEventListener('load', loadHandler);
  });
}