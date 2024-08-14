{
	"targets": [
		{
			"target_name": "addon",
			"conditions": [
        		['OS=="win"', {
					"sources": [
						"TrayWrapper.cpp",
						"TrayIcon.cpp"
					],
					"include_dirs": [
						"<!@(node -p \"require('node-addon-api').include\")"
					],
					'msbuild_settings': {
						"ClCompile": {
							"RuntimeLibrary": "MultiThreaded"
						}
					}
				}]
			]
		}
	]
}
