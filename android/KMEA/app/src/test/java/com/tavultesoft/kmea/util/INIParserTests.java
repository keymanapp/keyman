package com.tavultesoft.kmea.util;

import org.junit.Assert;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

public class INIParserTests {
	
	/*
	 * Note:  all current TODOs within this testing class represent corresponding test cases from
	 * 	      the iOS project.  They represent behaviors that need verification before we commit
	 *        further to them.
	 */
	
	/************** OVERALL PARSE TESTS *****************/
	@Test
	public void test_simple() {
		String text = "[section]\n" +
					  "key=value";
		
		Map<String, Map<String, List<String>>> result = INIParser.parse(text);
		
		Assert.assertNull(result.get(""));
    Assert.assertNotNull(result.get("section"));
		
		Assert.assertEquals("value", result.get("section").get("key").get(0));
	}
	
	@Test
  public void test_complex() {
		String text = "[foo]\n" + 
					  "key=value\n" +
					  "foo=bar\n" +
					  "[bar]\n" +
					  "bar=baz\n" +
					  "baz=foo";
		
		Map<String, Map<String, List<String>>> result = INIParser.parse(text);

    Assert.assertNull(result.get(""));
    Assert.assertNotNull(result.get("foo"));
    Assert.assertNotNull(result.get("bar"));
		
		Map<String, List<String>> foo = result.get("foo");
		Map<String, List<String>> bar = result.get("bar");

    Assert.assertEquals("value", foo.get("key").get(0));
    Assert.assertEquals("bar", foo.get("foo").get(0));
    Assert.assertEquals("baz", bar.get("bar").get(0));
    Assert.assertEquals("foo", bar.get("baz").get(0));
	}

	@Test
  public void test_sectionless_properties() {
		String text = "foo=bar\n" + 
					  "[section]\n" +
					  "key=value";
		
		Map<String, Map<String, List<String>>> result = INIParser.parse(text);

    Assert.assertNotNull(result.get(""));
    Assert.assertNotNull(result.get("section"));

    Assert.assertEquals("bar", result.get("").get("foo").get(0));
    Assert.assertEquals("value", result.get("section").get("key").get(0));
	}
	
	@Test
  public void test_empty_sections() {
		String text = "[section]\n" +
					  "key=value\n" +
					  "[empty section]\n" +
					  "[another empty section]\n";
		
		Map<String, Map<String, List<String>>> result = INIParser.parse(text);

    Assert.assertNull(result.get(""));
    Assert.assertNotNull(result.get("section"));
    Assert.assertNotNull(result.get("empty section"));
    Assert.assertNotNull(result.get("another empty section"));

    Assert.assertEquals("value", result.get("section").get("key").get(0));
		
		Map<String, List<String>> nil = new HashMap<String, List<String>>();

    Assert.assertEquals(nil, result.get("empty section"));
    Assert.assertEquals(nil, result.get("another empty section"));
	}
	
	// TODO:  testMultipleEqualSignsInProperty
	// TODO:  testQuotationMarksAndCommas
	// TODO:  testCommentsAndWhitespace
	
	@Test
  public void test_nil_text() {
		String text = "";
		
		Map<String, Map<String, List<String>>> result = INIParser.parse(text);
		
		Map<String, List<String>> nil = new HashMap<String, List<String>>();

    Assert.assertEquals(nil, result);
	}
	
	@Test
  public void test_nil_property() {
		String text = "foo=";
	
		Map<String, Map<String, List<String>>> result = INIParser.parse(text);

    Assert.assertNotNull(result.get(""));

    Assert.assertEquals("", result.get("").get("foo").get(0));
	}
	
	@Test
  public void test_sectionHeaderErrors() {
	  try {
      INIParser.parse("[section]invalid");
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }

    try {
      INIParser.parse("[section]]");
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }

    try {
      INIParser.parse("[section]foo]");
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }

    try {
      INIParser.parse("[invalid");
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }
	}
	
	@Test
  public void test_sectionDuplicates() {
		String text1 = "[foo]\n" + 
				  	   "[bar]\n" +
				  	   "[foo]";
		
		String text2 = "[section]\n" + 
			  	   	   "[section]";

    try {
      INIParser.parse(text1);
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }

    try {
      INIParser.parse(text2);
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }
	}
	
	@Test
  public void test_keyDuplicates() {
		String text1 = "[section]\n" + 
			  	       "foo=bar\n" +
			  	       "bar=baz\n" +
			  	       "foo=bar";
	
		String text2 = "[section]\n" + 
			  	       "foo=bar\n" +
			  	       "foo=bar";

    try {
      INIParser.parse(text1);
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }

    try {
      INIParser.parse(text2);
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }
	}
	
	// TODO:  testMissingEqualSignInProperty

	/************** UNQUOTING TESTS *****************/
	@Test
  public void test_unquotedQuoteCall() {
	  Assert.assertSame("foo", INIParser.unquote("foo"));
	}
	
	@Test
  public void test_plainQuoteCall() {
		Assert.assertEquals("foo", INIParser.unquote("\"foo\""));
	}
	
	@Test
  public void test_simpleQuoteErrors() {
    try {
      INIParser.unquote("\"foo");
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }

    try {
      INIParser.unquote("\"");
      Assert.fail("Failed to generate an INIParseException!");
    } catch (INIParseException e) { }
	}

	// TODO:  testUnquoteWithMultipleQuotes
	// TODO:  testUnquoteWithCharactersBeforeQuote
	// TODO:  testUnquoteWithCharactersAfterQuote

	/************** VALUE PARSING TESTS *****************/
	
	// TODO:  testComponents
	// TODO:  testEmptyComponents
	// TODO:  testComponentsWithUnterminatedQuote
	// TODO:  testComponentsWithTextAdjacentToQuote
}
