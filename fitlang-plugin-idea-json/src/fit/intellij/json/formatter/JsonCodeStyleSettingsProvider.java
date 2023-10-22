package fit.intellij.json.formatter;

import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.application.options.TabbedLanguageCodeStylePanel;
import com.intellij.psi.codeStyle.CodeStyleConfigurable;
import fit.intellij.json.JsonLanguage;
import com.intellij.lang.Language;
import com.intellij.openapi.options.Configurable;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsProvider;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author Mikhail Golubev
 */
public class JsonCodeStyleSettingsProvider extends CodeStyleSettingsProvider {

    @Override
    public CodeStyleConfigurable createConfigurable(@NotNull CodeStyleSettings settings, @NotNull CodeStyleSettings modelSettings) {
        CodeStyleConfigurable configurable = new CodeStyleAbstractConfigurable(settings, modelSettings, "FitLang") {
            @Override
            protected CodeStyleAbstractPanel createPanel(CodeStyleSettings settings) {
                final Language language = JsonLanguage.INSTANCE;
                final CodeStyleSettings currentSettings = getCurrentSettings();
                return new TabbedLanguageCodeStylePanel(language, currentSettings, settings) {
                    @Override
                    protected void initTabs(CodeStyleSettings settings) {
                        addIndentOptionsTab(settings);
                        addSpacesTab(settings);
                        addBlankLinesTab(settings);
                        addWrappingAndBracesTab(settings);
                    }
                };
            }

            @Nullable
            @Override
            public String getHelpTopic() {
                return "reference.settingsdialog.codestyle.json";
            }
        };
        return configurable;
    }

    @Nullable
    @Override
    public String getConfigurableDisplayName() {
        return JsonLanguage.INSTANCE.getDisplayName();
    }

    @Nullable
    @Override
    public CustomCodeStyleSettings createCustomSettings(CodeStyleSettings settings) {
        return new JsonCodeStyleSettings(settings);
    }
}