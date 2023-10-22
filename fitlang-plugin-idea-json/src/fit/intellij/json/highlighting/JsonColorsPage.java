package fit.intellij.json.highlighting;

import com.google.common.collect.ImmutableMap;
import com.intellij.icons.AllIcons;
import com.intellij.lang.Language;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.RainbowColorSettingsPage;
import com.intellij.psi.codeStyle.DisplayPriority;
import com.intellij.psi.codeStyle.DisplayPrioritySortable;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.JsonLanguage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

import static fit.intellij.json.highlighting.JsonSyntaxHighlighterFactory.*;

/**
 * @author Mikhail Golubev
 */
public class JsonColorsPage implements RainbowColorSettingsPage, DisplayPrioritySortable {
    private static final Map<String, TextAttributesKey> ourAdditionalHighlighting = ImmutableMap.of("propertyKey", JSON_PROPERTY_KEY);

    private static final AttributesDescriptor[] ourAttributeDescriptors = new AttributesDescriptor[]{
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.property.key"), JSON_PROPERTY_KEY),

            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.braces"), JSON_BRACES),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.brackets"), JSON_BRACKETS),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.comma"), JSON_COMMA),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.colon"), JSON_COLON),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.number"), JSON_NUMBER),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.string"), JSON_STRING),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.keyword"), JSON_KEYWORD),
            new AttributesDescriptor("MyKeyword", JsonSyntaxHighlighterFactory.MY_KEYWORD),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.line.comment"), JSON_LINE_COMMENT),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.block.comment"), JSON_BLOCK_COMMENT),
            //new AttributesDescriptor("", JSON_IDENTIFIER),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.valid.escape.sequence"), JSON_VALID_ESCAPE),
            new AttributesDescriptor(fit.intellij.json.JsonBundle.message("color.page.attribute.invalid.escape.sequence"), JSON_INVALID_ESCAPE),
    };

    @Nullable
    @Override
    public Icon getIcon() {
        return AllIcons.FileTypes.Json;
    }

    @NotNull
    @Override
    public SyntaxHighlighter getHighlighter() {
        return SyntaxHighlighterFactory.getSyntaxHighlighter(fit.intellij.json.JsonLanguage.INSTANCE, null, null);
    }

    @NotNull
    @Override
    public String getDemoText() {
        return "{\n" +
                "  // Line comments are not included in standard but nonetheless allowed.\n" +
                "  <propertyKey>\"uniform node identifier\"</propertyKey>: \"loop\",\n" +
                "  <propertyKey>\"property name\"</propertyKey>: [true, false, null],\n" +
                "  <propertyKey>\"child node\"</propertyKey>: [{\n" +
                "    <myKeyword>\"uni\"</myKeyword>: \"hello\",\n" +
                "    \"uni\": \"hello\",\n" +
                "    <propertyKey>\"who\"</propertyKey>: \"fit\",\n" +
                "    \"input\": \"fit\",\n" +
                "    \"id\": \"10001\",\n" +
                "    \"name\": \"hello\",\n" +
                "    <propertyKey>\"no escapes\"</propertyKey>: \"pseudopolinomiality\"\n" +
                "    <propertyKey>\"valid escapes\"</propertyKey>: \"C-style\\r\\n and unicode\\u0021\",\n" +
                "    <propertyKey>\"illegal escapes\"</propertyKey>: \"\\0377\\x\\\"\n" +
                "  }],\n" +
                "  <propertyKey>\"some numbers\"</propertyKey>: [\n" +
                "    42,\n" +
                "    -0.0e-0,\n" +
                "    6.626e-34\n" +
                "  ] \n" +
                "}";
    }

    @Nullable
    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return ourAdditionalHighlighting;
    }

    @Override
    public AttributesDescriptor @NotNull [] getAttributeDescriptors() {
        return ourAttributeDescriptors;
    }

    @Override
    public ColorDescriptor @NotNull [] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @NotNull
    @Override
    public String getDisplayName() {
        return JsonBundle.message("settings.display.name.json");
    }

    @Override
    public DisplayPriority getPriority() {
        return DisplayPriority.LANGUAGE_SETTINGS;
    }

    @Override
    public boolean isRainbowType(TextAttributesKey type) {
        return JsonSyntaxHighlighterFactory.MY_KEYWORD.equals(type)
                || JSON_PROPERTY_KEY.equals(type)
                || JSON_BRACES.equals(type)
                || JSON_BRACKETS.equals(type)
                || JSON_STRING.equals(type)
                || JSON_NUMBER.equals(type)
                || JSON_KEYWORD.equals(type);
    }

    @Nullable
    @Override
    public Language getLanguage() {
        return JsonLanguage.INSTANCE;
    }
}
