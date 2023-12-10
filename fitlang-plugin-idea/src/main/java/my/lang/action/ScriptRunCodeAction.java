package my.lang.action;

import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.project.Project;

import java.util.Map;

import static my.lang.MyLanguage.LANG_NAME;
import static my.lang.MyLanguage.LANG_STRING_LOGO;

/**
 * @author yanchangyou
 */
public class ScriptRunCodeAction extends RunCodeAction {

    @Override
    public String getLanguageName() {
        return LANG_NAME;
    }

    @Override
    public String getLogoString() {
        return LANG_STRING_LOGO;
    }

    @Override
    public Map<Project, ConsoleView> getProjectConsoleViewMap() {
        return projectConsoleViewMap;
    }

    protected ScriptRunCodeAction() {

    }

    protected ScriptRunCodeAction(String title) {
        super(title);
    }
}