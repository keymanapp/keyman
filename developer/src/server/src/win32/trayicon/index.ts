import * as os from 'os';
import { createRequire } from 'node:module';
const require = createRequire(import.meta.url);
const { CTrayIconContainer } = require(os.arch() == 'ia32' ? "./addon" : "./addon.x64");

class WindowsTrayicon {
	__itemCallbacks: any[];
	__icon: any;
	__trayTitle: any;
	__menuItems: any;
	__nativeTray: any;


	constructor(options: any) {
		this.__itemCallbacks = [];
		this.__icon = options.icon;
		this.__trayTitle = options.title || "";
		this.__menuItems = options.menu || [];

		this.__nativeTray = new CTrayIconContainer();
		for (const item of this.__menuItems) {
			this.__nativeTray.AddMenuItem(item.id, item.caption);
		}
		this.__nativeTray.OnMenuItem((id: any) => {
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
	item(cb: any) {
		if ("function" === typeof cb) {
			this.__itemCallbacks.push(cb);
		}
	}
	balloon(title: any, text: any, timeout = 5000) {
		return new Promise((resolve) => {
			this.__nativeTray.ShowBalloon(title, text, timeout, () => {
				resolve(null);
			})
		});
	}
	exit() {
		this.__nativeTray.Stop();
	}
}

export default WindowsTrayicon;
