#!/usr/bin/python3

import logging
import subprocess
import urllib.parse
import webbrowser


def check_mime_type(webview, frame, request, mimetype, policy_decision):
    """Handle downloads and PDF files."""
    if mimetype == 'application/pdf':
        logging.info("check_mime_type: Download and run %s", request.get_uri())
        parse_url = urllib.parse.urlparse(request.get_uri())
        if parse_url.scheme == "file":
            subprocess.call(['xdg-open', parse_url.path])
        else:
            webbrowser.open(request.get_uri())
        policy_decision.ignore()
        return True
    return False
