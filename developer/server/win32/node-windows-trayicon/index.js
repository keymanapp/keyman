const { CTrayIconContainer } = require("bindings")("addon");

class WindowsTrayicon {
	constructor(options) {
		this.__itemCallbacks = [];
		this.__icon = options.icon;
		this.__trayTitle = options.title || "";
		this.__menuItems = options.menu || [];

		this.__nativeTray = new CTrayIconContainer();
		for (const item of this.__menuItems) {
			this.__nativeTray.AddMenuItem(item.id, item.caption);
		}
		this.__nativeTray.OnMenuItem((id) => {
			for (const cb of this.__itemCallbacks) {
				cb(id);
			}
		})
		this.__nativeTray.SetTitle(this.__trayTitle);
		if(this.__icon && "string" === typeof this.__icon){
			this.__nativeTray.SetIconPath(this.__icon);
		}
		this.__nativeTray.Start();
	}
	item(cb) {
		if ("function" === typeof cb) {
			this.__itemCallbacks.push(cb);
		}
	}
	balloon(title, text, timeout = 5000) {
		return new Promise((resolve) => {
			this.__nativeTray.ShowBalloon(title, text, timeout, () => {
				resolve();
			})
		});
	}
	exit() {
		this.__nativeTray.Stop();
	}
}

module.exports = WindowsTrayicon;
