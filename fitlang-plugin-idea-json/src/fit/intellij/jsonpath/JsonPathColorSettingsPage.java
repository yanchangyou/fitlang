// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.jsonpath;

import com.intellij.icons.AllIcons;
import fit.intellij.json.JsonBundle;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import com.intellij.openapi.util.NlsContexts;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.HashMap;
import java.util.Map;

final class JsonPathColorSettingsPage implements ColorSettingsPage {

  private final AttributesDescriptor[] myAttributesDescriptors = new AttributesDescriptor[]{
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.keyword"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_KEYWORD),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.identifier"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_IDENTIFIER),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.string"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_STRING),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.number"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_NUMBER),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.boolean"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_BOOLEAN),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.brackets"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_BRACKETS),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.braces"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_BRACES),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.colon"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_COLON),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.comma"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_COMMA),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.dot"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_DOT),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.operation.sign"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_OPERATIONS),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.parentheses"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_PARENTHESES),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.context"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_CONTEXT),
    new AttributesDescriptor(JsonBundle.message("jsonpath.color.page.function"), fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_FUNCTION_CALL)
  };

  @Override
  public @NotNull @NlsContexts.ConfigurableName String getDisplayName() {
    return JsonBundle.message("settings.display.name.jsonpath");
  }

  @Override
  public @NotNull Icon getIcon() {
    return AllIcons.FileTypes.Json;
  }

  @Override
  public @NotNull SyntaxHighlighter getHighlighter() {
    return new fit.intellij.jsonpath.JsonPathSyntaxHighlighter();
  }

  @Override
  public @NotNull Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
    Map<String, TextAttributesKey> additionalMap = new HashMap<>();
    additionalMap.put("functionCall", fit.intellij.jsonpath.JsonPathSyntaxHighlighter.JSONPATH_FUNCTION_CALL);
    return additionalMap;
  }

  @Override
  public @NonNls @NotNull String getDemoText() {
    return """
      $.store.book[0, 2].title
      $['store']["book"][0]['title']
      $.authors[*].publications[:10]
      $.text.<functionCall>concat</functionCall>("-", "some")
      $.text[?(@ =~ /9.*9/ && $.enabled == false)]
      $..book[?($.count > @['stats counter'].<functionCall>sum</functionCall>())]
      @.sales[?(@.active == true || $.library != null)]
      $.store.bicycle[?(@.extra == { 'x': [{}, {'key' : 'value'}] })]
      """;
  }

  @Override
  public AttributesDescriptor @NotNull [] getAttributeDescriptors() {
    return myAttributesDescriptors;
  }

  @Override
  public ColorDescriptor @NotNull [] getColorDescriptors() {
    return ColorDescriptor.EMPTY_ARRAY;
  }
}
