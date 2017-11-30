package com.tavultesoft.kmea.util;

import java.util.Map;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * A custom, lightweight INI parser designed initially for use with Keyman's .kmp and .inf files.
 * @author jahorton
 *
 */
public class INIParser {
	public static Map<String, Map<String, List<String>>> parse(String _string) throws INIParseException {
		Map<String, Map<String, List<String>>> sections = new HashMap<String, Map<String, List<String>>>();
		
		String[] linesArray = _string.split("\n");
		ArrayList<String> lines = new ArrayList<String>(linesArray.length);
		
		// Perform comment & whitespace filtering.
		for(int i=0; i < linesArray.length; i++) {
			String line = linesArray[i].trim();
			if(!(line.equals("") || line.charAt(0) == ';')) {
				lines.add(line);
			}
		}
		
		// Allow a plain default section - if it goes unutilized, we can remove it later.
		String currentSectionName = "";
		Map<String, List<String>> currentSection = new HashMap<String, List<String>>();
		sections.put(currentSectionName, currentSection);
		
		// Main parsing loop.
		for(String line:lines) {
			if(line.charAt(0) == '[') {
				currentSectionName = unquote_helper(line, '[', ']', 
						"A section definition is missing its closing bracket",
						"Improper section header definition - extra characters detected");
						
				
				if(sections.containsKey(currentSectionName)) {
					throw new INIParseException("Section \"" + currentSectionName + "\" has a duplicate entry!");
				}
				
				currentSection = new HashMap<String, List<String>>();
				sections.put(currentSectionName, currentSection);
			} else if(line.indexOf('=') != -1) {
				int eq = line.indexOf('=');
				
				String key = line.substring(0, eq).trim();
				String values = line.substring(eq + 1).trim();
				
				if(currentSection.containsKey(key)) {
					throw new INIParseException("Section property duplicated for key \"" + key + "\" in section \"" + currentSectionName + "\".");
				}
				
				currentSection.put(key, components(values));
			}
		}
		
		// If the default section is empty, remove it.
		if(sections.get("").isEmpty()) {
			sections.remove("");
		}
		
		return sections;
	}
	
	public static List<String> components (String text) throws INIParseException {
		List<String> values = new ArrayList<String>();
		
		text = text.trim();
		Scanner tokens = new Scanner(text);
		
		try {
			while(tokens.hasNext()) {
				// TODO:  Determine patterns, do tokenization, determine actual relevant values.
				if(tokens.hasNext("")) {
					// TODO:
				}
				return new ArrayList<String>();
			}
		} finally {
			tokens.close();
		}
		
		return values;
	}
	
	static String unquote(String text) throws INIParseException {
		// Filter both types of quotes.
		String val = unquote_helper(text, '\'', '\'', 
				"A quoted value was left unterminated", 
				"Illegal text adjacent to ending quote");
		
		if(val == text) { // Reference equality is fine for this test, given unquote_helper's implementation.
			return unquote_helper(val, '\"', '\"',
				"A quoted value was left unterminated", 
				"Illegal text adjacent to ending quote");
		} else {
			return val;
		}

	}
	
	static String unquote_helper(String text, char open, char closed, String unclosedMsg, String malformedMsg) throws INIParseException {
		if(text.charAt(0) == open) {
			text = text.substring(1);
			int closeIndex = text.indexOf(closed);
			
			if(closeIndex == -1) {
				throw new INIParseException(unclosedMsg + ": " + text);
			} else if(closeIndex != text.length() - 1) {
				throw new INIParseException(malformedMsg + ": " + text);
			}
			
			return text.substring(0, closeIndex); 
		} else {
			return text;
		}
	}
}
