exports.LMLayer = class LMLayer {
  /**
   *
   */
  async predictWithContext({_transform, _context, _token}) {
    return [
      { insert: 'Derek', deleteLeft: 1, deleteRight: 0 },
    ];
  }
};
