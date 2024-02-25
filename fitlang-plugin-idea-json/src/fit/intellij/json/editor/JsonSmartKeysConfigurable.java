//// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
//package fit.intellij.json.editor;
//
//import com.intellij.openapi.options.ConfigurableBuilder;
//import com.intellij.openapi.options.SearchableConfigurable;
//import fit.intellij.json.JsonBundle;
//import org.jetbrains.annotations.Nls;
//import org.jetbrains.annotations.NotNull;
//
//public class JsonSmartKeysConfigurable extends ConfigurableBuilder implements SearchableConfigurable {
//  public JsonSmartKeysConfigurable() {
//    fit.intellij.json.editor.JsonEditorOptions settings = JsonEditorOptions.getInstance();
//    if (settings == null) return;
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.insert.missing.comma.on.enter"),
//             () -> settings.COMMA_ON_ENTER,
//             v -> settings.COMMA_ON_ENTER = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.insert.missing.comma.after.matching.braces.and.quotes"),
//             () -> settings.COMMA_ON_MATCHING_BRACES,
//             v -> settings.COMMA_ON_MATCHING_BRACES = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.automatically.manage.commas.when.pasting.json.fragments"),
//             () -> settings.COMMA_ON_PASTE,
//             v -> settings.COMMA_ON_PASTE = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.escape.text.on.paste.in.string.literals"),
//             () -> settings.ESCAPE_PASTED_TEXT,
//             v -> settings.ESCAPE_PASTED_TEXT = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.automatically.add.quotes.to.property.names.when.typing.comma"),
//             () -> settings.AUTO_QUOTE_PROP_NAME,
//             v -> settings.AUTO_QUOTE_PROP_NAME = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.automatically.add.whitespace.when.typing.comma.after.property.names"),
//             () -> settings.AUTO_WHITESPACE_AFTER_COLON,
//             v -> settings.AUTO_WHITESPACE_AFTER_COLON = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.automatically.move.comma.after.the.property.name.if.typed.inside.quotes"),
//             () -> settings.COLON_MOVE_OUTSIDE_QUOTES,
//             v -> settings.COLON_MOVE_OUTSIDE_QUOTES = v);
//    checkBox(fit.intellij.json.JsonBundle.message("settings.smart.keys.automatically.move.comma.after.the.property.value.or.array.element.if.inside.quotes"),
//             () -> settings.COMMA_MOVE_OUTSIDE_QUOTES,
//             v -> settings.COMMA_MOVE_OUTSIDE_QUOTES = v);
//  }
//
//  @Nls(capitalization = Nls.Capitalization.Title)
//  @Override
//  public String getDisplayName() {
//    return JsonBundle.message("configurable.JsonSmartKeysConfigurable.display.name");
//  }
//
//  @NotNull
//  @Override
//  public String getId() {
//    return "editor.preferences.jsonOptions";
//  }
//}
