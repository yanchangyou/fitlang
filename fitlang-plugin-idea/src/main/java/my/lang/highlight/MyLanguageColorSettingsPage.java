package my.lang.highlight;

import com.google.common.collect.ImmutableMap;
import com.intellij.icons.AllIcons;
import com.intellij.json.JsonLanguage;
import com.intellij.json.highlighting.JsonColorsPage;
import com.intellij.lang.Language;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.psi.codeStyle.DisplayPriority;
import my.lang.MyLanguage;
import my.lang.MyLanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

import static com.intellij.json.highlighting.JsonSyntaxHighlighterFactory.*;
import static com.intellij.json.highlighting.JsonSyntaxHighlighterFactory.JSON_KEYWORD;

/**
 * @author yanchangyou
 */
public class MyLanguageColorSettingsPage extends JsonColorsPage {

    private static final AttributesDescriptor[] ourAttributeDescriptors = new AttributesDescriptor[]{
            new AttributesDescriptor("Property key", JSON_PROPERTY_KEY),

            new AttributesDescriptor("Braces", JSON_BRACES),
            new AttributesDescriptor("Brackets", JSON_BRACKETS),
            new AttributesDescriptor("Comma", JSON_COMMA),
            new AttributesDescriptor("Colon", JSON_COLON),
            new AttributesDescriptor("Number", JSON_NUMBER),
            new AttributesDescriptor("String", JSON_STRING),
            new AttributesDescriptor("Keyword", JSON_KEYWORD),
            new AttributesDescriptor("Line comment", JSON_LINE_COMMENT),
            new AttributesDescriptor("Block comment", JSON_IDENTIFIER),
            new AttributesDescriptor("Uniform Node Identifier", JSON_KEYWORD),
            new AttributesDescriptor("Valid escape sequence", JSON_VALID_ESCAPE),
            new AttributesDescriptor("Invalid escape sequence", JSON_INVALID_ESCAPE),
    };

    @Nullable
    @Override
    public Icon getIcon() {
        return MyLanguageFileType.INSTANCE.getIcon();
    }

    @NotNull
    @Override
    public SyntaxHighlighter getHighlighter() {
        return SyntaxHighlighterFactory.getSyntaxHighlighter(MyLanguageFileType.INSTANCE, null, null);
    }

    @NotNull
    @Override
    public String getDemoText() {
        return "{\n" +
                "  // Line comments are not included in standard but nonetheless allowed.\n" +
                "  <propertyKey>\"uniform node identifier\"</propertyKey>: \"loop\",\n" +
                "  <propertyKey>\"property name\"</propertyKey>: [true, false, null],\n" +
                "  <propertyKey>\"child node\"</propertyKey>: [{\n" +
                "    <propertyKey>\"uniform node identifier\"</propertyKey>: \"hello\",\n" +
                "    <propertyKey>\"who\"</propertyKey>: \"fit\",\n" +
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


    @NotNull
    @Override
    public AttributesDescriptor[] getAttributeDescriptors() {
        return ourAttributeDescriptors;
    }

    @NotNull
    @Override
    public String getDisplayName() {
        return "FitLang";
    }

    @Nullable
    @Override
    public Language getLanguage() {
        return MyLanguage.INSTANCE;
    }
}
