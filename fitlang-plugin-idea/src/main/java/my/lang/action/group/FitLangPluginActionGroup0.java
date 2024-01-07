package my.lang.action.group;

import com.intellij.openapi.actionSystem.DataContext;
import my.lang.action.FitLangPluginActionGroup;
import org.jetbrains.annotations.NotNull;

public class FitLangPluginActionGroup0 extends FitLangPluginActionGroup {

    public boolean canBePerformed(@NotNull DataContext context) {
        return actionConfig != null;
    }

    public FitLangPluginActionGroup0() {
        super();
    }

    @Override
    public String getGroupName() {
        return "FitActionGroup0";
    }
}