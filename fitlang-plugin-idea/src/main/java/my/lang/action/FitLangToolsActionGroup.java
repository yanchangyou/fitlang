package my.lang.action;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.openapi.editor.Editor;

public class FitLangToolsActionGroup extends DefaultActionGroup {
    @Override
    public void update(AnActionEvent event) {
        // Enable/disable depending on whether a user is editing
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        event.getPresentation().setEnabled(true);
    }
}