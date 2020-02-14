-- main.scpt
-- Install Keyman
--
-- Copyright 2020 SIL International. All rights reserved.

-- This script automates the installation of Keyman on macOS. It is intended
-- as a stop-gap for a full-featured installer which provides clearer 
-- instructions to the end user.
-- The script application expects a notarized Keyman.app to be placed inside
-- its folder in order for installation to succeed.

set DLOG_TARGETS to {"syslog"}

script installKeyman
	
	#
	# Display welcome dialog
	#
	
	dlog("### Starting Keyman installer ###")
	try
		-- dialog
		set dialogReply to display dialog Â
			"This app will help you install Keyman on your Mac.

This installer will copy Keyman into your Input Methods folder, and configure Keyman as an input source. After installation, Keyman will be found in the menu bar up near the clock." buttons {"Install", "Exit"} Â
			default button Â
			"Install" cancel button Â
			"Exit" with title Â
			Â
				"Install Keyman for macOS" with icon Â
			"applet.icns"
		
	on error errText number errNum
		if (errNum is equal to -128) then
			-- User cancelled.
			dlog("User cancelled at Welcome dialog")
			tell me to quit
		end if
	end try
	
	#
	# Tell application "Keyman" to quit (if running, for upgrade; ignore result)
	#
	
	dlog("### Shutting down existing instances of Keyman ###")
	set scriptResult to do shell script "pkill Keyman || true"
	dlog(scriptResult)
	
	#
	# Remove old version of Keyman (ignore result)
	#
	
	dlog("### Removing old version of Keyman ###")
	set scriptResult to do shell script "rm -vrf ~/Library/Input\\ Methods/Keyman.app || true"
	dlog(scriptResult)
	
	#
	# Install Keyman (copy app folder)	
	#
	
	set scriptContainer to quoted form of POSIX path of (path to current application) as string
	dlog("### Copying new Keyman from " & scriptContainer & "Keyman.app to ~/Library/Input\\ Methods ###")
	set cmd to "cp -vR " & scriptContainer & "Contents/MacOS/Keyman.app ~/Library/Input\\ Methods/"
	set scriptResult to do shell script cmd
	dlog(scriptResult)
	
	#
	# remove quarantine extended attribute
	#
	
	dlog("### Removing quarantine attribute on installed Keyman.app ###")
	set scriptResult to do shell script "xattr -d -r com.apple.quarantine ~/Library/Input\\ Methods/Keyman.app"
	dlog(scriptResult)
	
	#
	# Enable Keyman in Privacy / Input Monitoring
	# TODO: this will come in a future version of the installer.
	# In the current version, the user will be prompted post-install
	#
	(*
	dlog("### Helping user enable Keyman in Input Monitoring ###")
	
	try
		tell application "System Preferences"
			set the current pane to pane id "com.apple.preference.security"
			reveal anchor "Privacy" of pane id "com.apple.preference.security"
			activate
		end tell
	on error errText number errNum
		dlog("An error " & errText & " (" & errNum & ") occurred trying to open System Preferences")
		if (errNum is equal to -1743) then
			-- User did not give permission to app, let's show the dialog anyway.
			dlog("Attempting to continue")
		else
			display dialog "An error occurred trying to open System Preferences: " & errNum & return & errText
		end if
	end try
	
	tell me to activate
	
	try
		set dialogReply to display dialog Â
			"In the Privacy pane of Security & Privacy in System Preferences, select 'Input Monitoring', " & Â
			"then click the lock to make changes, and make sure Keyman in the list is checked." & return & return & Â
			"Once Keyman is checked, click OK here to continue." buttons {"OK", "Exit"} Â
			default button Â
			"OK" cancel button Â
			"Exit" with title Â
			Â
				Â
					"Install Keyman for macOS" with icon Â
			"applet.icns"
	on error errText number errNum
		if (errNum is equal to -128) then
			-- User cancelled.
			dlog("User cancelled at Input Monitoring")
			tell me to quit
		end if
	end try
	*)
	
	#
	# Enable Keyman as an input method
	#
	
	dlog("### Helping user add Keyman to Input Methods ###")
	set cmd to scriptContainer & "Contents/MacOS/textinputsource -e Keyman"
	set scriptResult to do shell script cmd
	dlog(scriptResult)
	
	# TODO: check the output result and if Keyman is not found, then
	# we go to troubleshooting mode?
	
	(*	
	tell application "System Preferences"
		set the current pane to pane id "com.apple.preference.keyboard"
		# set r to get the name of every anchor of pane id "com.apple.preference.keyboard"
		reveal anchor "InputSources" of pane id "com.apple.preference.keyboard"
		activate
	end tell
	
	tell me to activate
	
	try
		set dialogReply to display dialog Â
			"In the Input Sources pane of Keyboard Preferences, click + " & Â
			"to add Keyman, which will be under 'Multiple Languages' in the " & Â
			Â
				"list presented to you.

Once you have added Keyman, click OK here to continue." buttons {"OK", "Exit"} Â
			default button Â
			"OK" cancel button Â
			"Exit" with title Â
			Â
				Â
					"Install Keyman for macOS" with icon Â
			"applet.icns"
	on error errText number errNum
		if (errNum is equal to -128) then
			-- User cancelled.
			dlog("User cancelled at Input Sources")
			tell me to quit
		end if
	end try
	*)
	
	#
	# Success!
	#
	
	display dialog "Keyman has been successfully installed!

You can now select Keyman from the " & Â
		"Input Sources menu in the menu bar, up near the clock.

" & Â
		"You may be prompted to allow Keyman to receive keystrokes from any application; when prompted, follow the instructions on screen to allow Keyman to run.

" & Â
		"When ready, select Keyman from the Input Sources menu, then open the " & Â
		Â
			"Input Sources menu once more and select Keyman Configuration to add your first keyboard." buttons {"OK"} with icon "applet.icns"
	
	dlog("### Successful installation ###")
	
	tell me to quit
	# display alert r
end script

run installKeyman

# From: https://stackoverflow.com/a/21341372/1836776
# Logs a text representation of the specified object or objects, which may be of any type, typically for debugging.
# Works hard to find a meaningful text representation of each object.
# SYNOPSIS
#   dlog(anyObjOrListOfObjects)
# USE EXAMPLES
#   dlog("before")  # single object
#     dlog({ "front window: ", front window }) # list of objects
# SETUP
#   At the top of your script, define global variable DLOG_TARGETS and set it to a *list* of targets (even if you only have 1 target).
#     set DLOG_TARGETS to {} # must be a list with any combination of: "log", "syslog", "alert", <posixFilePath>
#   An *empty* list means that logging should be *disabled*.
#   If you specify a POSIX file path, the file will be *appended* to; variable references in the path
#   are allowed, and as a courtesy the path may start with "~" to refer to your home dir.
#   Caveat: while you can *remove* the variable definition to disable logging, you'll take an additional performance hit.
# SETUP EXAMPLES
#    For instance, to use both AppleScript's log command *and* display a GUI alert, use:
#       set DLOG_TARGETS to { "log", "alert" }
# Note: 
#   - Since the subroutine is still called even when DLOG_TARGETS is an empty list, 
#     you pay a performancy penalty for leaving dlog() calls in your code.
#   - Unlike with the built-in log() method, you MUST use parentheses around the parameter.
#   - To specify more than one object, pass a *list*. Note that while you could try to synthesize a single
#     output string by concatenation yourself, you'd lose the benefit of this subroutine's ability to derive
#     readable text representations even of objects that can't simply be converted with `as text`.
on dlog(anyObjOrListOfObjects)
	global DLOG_TARGETS
	try
		if length of DLOG_TARGETS is 0 then return
	on error
		return
	end try
	# The following tries hard to derive a readable representation from the input object(s).
	if class of anyObjOrListOfObjects is not list then set anyObjOrListOfObjects to {anyObjOrListOfObjects}
	local lst, i, txt, errMsg, orgTids, oName, oId, prefix, logTarget, txtCombined, prefixTime, prefixDateTime
	set lst to {}
	repeat with anyObj in anyObjOrListOfObjects
		set txt to ""
		repeat with i from 1 to 2
			try
				if i is 1 then
					if class of anyObj is list then
						set {orgTids, AppleScript's text item delimiters} to {AppleScript's text item delimiters, {", "}} # '
						set txt to ("{" & anyObj as string) & "}"
						set AppleScript's text item delimiters to orgTids # '
					else
						set txt to anyObj as string
					end if
				else
					set txt to properties of anyObj as string
				end if
			on error errMsg
				# Trick for records and record-*like* objects:
				# We exploit the fact that the error message contains the desired string representation of the record, so we extract it from there. This (still) works as of AS 2.3 (OS X 10.9).
				try
					set txt to do shell script "egrep -o '\\{.*\\}' <<< " & quoted form of errMsg
				end try
			end try
			if txt is not "" then exit repeat
		end repeat
		set prefix to ""
		if class of anyObj is not in {text, integer, real, boolean, date, list, record} and anyObj is not missing value then
			set prefix to "[" & class of anyObj
			set oName to ""
			set oId to ""
			try
				set oName to name of anyObj
				if oName is not missing value then set prefix to prefix & " name=\"" & oName & "\""
			end try
			try
				set oId to id of anyObj
				if oId is not missing value then set prefix to prefix & " id=" & oId
			end try
			set prefix to prefix & "] "
			set txt to prefix & txt
		end if
		set lst to lst & txt
	end repeat
	set {orgTids, AppleScript's text item delimiters} to {AppleScript's text item delimiters, {" "}} # '
	set txtCombined to lst as string
	set prefixTime to "[" & time string of (current date) & "] "
	set prefixDateTime to "[" & short date string of (current date) & " " & text 2 thru -1 of prefixTime
	set AppleScript's text item delimiters to orgTids # '
	# Log the result to every target specified.
	repeat with logTarget in DLOG_TARGETS
		if contents of logTarget is "log" then
			log prefixTime & txtCombined
		else if contents of logTarget is "alert" then
			display alert prefixTime & txtCombined
		else if contents of logTarget is "syslog" then
			# display alert "logger -t " & quoted form of ("AS: " & (name of me)) & " " & quoted form of txtCombined
			set res to do shell script "logger -t " & quoted form of ("AS: " & (name of me)) & " " & quoted form of txtCombined
			# display alert res
		else # assumed to be a POSIX file path to *append* to.
			set fpath to contents of logTarget
			if fpath starts with "~/" then set fpath to "$HOME/" & text 3 thru -1 of fpath
			do shell script "printf '%s\\n' " & quoted form of (prefixDateTime & txtCombined) & " >> \"" & fpath & "\""
		end if
	end repeat
end dlog