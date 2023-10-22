package fit.intellij.json.highlighting;

import com.google.common.collect.ImmutableMap;
import com.intellij.icons.AllIcons;
import fit.intellij.json.JsonLanguage;
import com.intellij.lang.Language;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.RainbowColorSettingsPage;
import com.intellij.psi.codeStyle.DisplayPriority;
import com.intellij.psi.codeStyle.DisplayPrioritySortable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

/**
 * @author Mikhail Golubev
 */
public class JsonColorsPage implements RainbowColorSettingsPage, DisplayPrioritySortable {
    private static final Map<String, TextAttributesKey> ourAdditionalHighlighting = ImmutableMap.of(
            "propertyKey", JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY,
            "myKeyword", JsonSyntaxHighlighterFactory.MY_KEYWORD
    );

    private static final AttributesDescriptor[] ourAttributeDescriptors = new AttributesDescriptor[]{
            new AttributesDescriptor("Property key", JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY),

            new AttributesDescriptor("Braces", JsonSyntaxHighlighterFactory.JSON_BRACES),
            new AttributesDescriptor("Brackets", JsonSyntaxHighlighterFactory.JSON_BRACKETS),
            new AttributesDescriptor("Comma", JsonSyntaxHighlighterFactory.JSON_COMMA),
            new AttributesDescriptor("Colon", JsonSyntaxHighlighterFactory.JSON_COLON),
            new AttributesDescriptor("Number", JsonSyntaxHighlighterFactory.JSON_NUMBER),
            new AttributesDescriptor("String", JsonSyntaxHighlighterFactory.JSON_STRING),
            new AttributesDescriptor("Keyword", JsonSyntaxHighlighterFactory.JSON_KEYWORD),
            new AttributesDescriptor("MyKeyword", JsonSyntaxHighlighterFactory.MY_KEYWORD),
            new AttributesDescriptor("Line comment", JsonSyntaxHighlighterFactory.JSON_LINE_COMMENT),
            new AttributesDescriptor("Block comment", JsonSyntaxHighlighterFactory.JSON_BLOCK_COMMENT),
            //new AttributesDescriptor("", JSON_IDENTIFIER),
            new AttributesDescriptor("Valid escape sequence", JsonSyntaxHighlighterFactory.JSON_VALID_ESCAPE),
            new AttributesDescriptor("Invalid escape sequence", JsonSyntaxHighlighterFactory.JSON_INVALID_ESCAPE),
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

    @NotNull
    @Override
    public AttributesDescriptor[] getAttributeDescriptors() {
        return ourAttributeDescriptors;
    }

    @NotNull
    @Override
    public ColorDescriptor[] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @NotNull
    @Override
    public String getDisplayName() {
        return "FitLang";
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
