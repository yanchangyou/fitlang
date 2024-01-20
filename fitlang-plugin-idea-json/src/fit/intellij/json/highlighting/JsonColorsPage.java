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

/**
 * @author Mikhail Golubev
 */
public class JsonColorsPage implements RainbowColorSettingsPage, DisplayPrioritySortable {
    private static final Map<String, TextAttributesKey> ourAdditionalHighlighting = ImmutableMap.of("propertyKey", JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY);

    private static final AttributesDescriptor[] ourAttributeDescriptors = new AttributesDescriptor[]{
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.property.key"), JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY),

            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.braces"), JsonSyntaxHighlighterFactory.JSON_BRACES),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.brackets"), JsonSyntaxHighlighterFactory.JSON_BRACKETS),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.comma"), JsonSyntaxHighlighterFactory.JSON_COMMA),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.colon"), JsonSyntaxHighlighterFactory.JSON_COLON),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.number"), JsonSyntaxHighlighterFactory.JSON_NUMBER),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.string"), JsonSyntaxHighlighterFactory.JSON_STRING),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.keyword"), JsonSyntaxHighlighterFactory.JSON_KEYWORD),
            new AttributesDescriptor("MyKeyword", JsonSyntaxHighlighterFactory.MY_KEYWORD),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.line.comment"), JsonSyntaxHighlighterFactory.JSON_LINE_COMMENT),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.block.comment"), JsonSyntaxHighlighterFactory.JSON_BLOCK_COMMENT),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.valid.escape.sequence"), JsonSyntaxHighlighterFactory.JSON_VALID_ESCAPE),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.invalid.escape.sequence"), JsonSyntaxHighlighterFactory.JSON_INVALID_ESCAPE),
            new AttributesDescriptor(JsonBundle.messagePointer("color.page.attribute.parameter"), JsonSyntaxHighlighterFactory.JSON_PARAMETER)
    };

    @Nullable
    @Override
    public Icon getIcon() {
        return AllIcons.FileTypes.Json;
    }

    @NotNull
    @Override
    public SyntaxHighlighter getHighlighter() {
        return SyntaxHighlighterFactory.getSyntaxHighlighter(JsonLanguage.INSTANCE, null, null);
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
                || JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY.equals(type)
                || JsonSyntaxHighlighterFactory.JSON_BRACES.equals(type)
                || JsonSyntaxHighlighterFactory.JSON_BRACKETS.equals(type)
                || JsonSyntaxHighlighterFactory.JSON_STRING.equals(type)
                || JsonSyntaxHighlighterFactory.JSON_NUMBER.equals(type)
                || JsonSyntaxHighlighterFactory.JSON_KEYWORD.equals(type);
    }

    @Nullable
    @Override
    public Language getLanguage() {
        return JsonLanguage.INSTANCE;
    }
}
