package fit.lang.plugin.json.ide;

import fit.lang.plugin.json.ide.node.*;
import fit.lang.plugin.json.ide.node.message.*;

import static fit.lang.plugin.json.JsonDynamicFlowExecuteEngine.register;


public class FitPluginInit {

    public static void init() {

        //ide
        register("readEditor", ReadEditorJsonExecuteNode.class);
        register("writeEditor", WriteEditorJsonExecuteNode.class);
        register("openWebPage", OpenWebPageJsonExecuteNode.class);
        register("showHtml", ShowHtmlJsonExecuteNode.class);
        register("showJsonPage", ShowJsonPageJsonExecuteNode.class);
        register("showConfig", ShowConfigJsonExecuteNode.class);
        register("readConfig", ReadConfigJsonExecuteNode.class);
        register("showGlobalConfigDialog", ShowGlobalConfigDialogJsonExecuteNode.class);
        register("chooseFile", ChooseFileJsonExecuteNode.class);
        register("showInfoMessage", ShowInfoMessageJsonExecuteNode.class);
        register("showWarningMessage", ShowWarningMessageJsonExecuteNode.class);
        register("showErrorMessage", ShowErrorMessageJsonExecuteNode.class);
        register("showInputDialog", ShowInputDialogJsonExecuteNode.class);
        register("showOkCancelDialog", ShowOkCancelDialogJsonExecuteNode.class);
        register("showYesNoCancelDialog", ShowYesNoCancelDialogJsonExecuteNode.class);
        register("showCheckboxOkCancelDialog", ShowCheckboxOkCancelDialogJsonExecuteNode.class);
        register("showPasswordDialog", ShowPasswordDialogJsonExecuteNode.class);
        register("showDiff", ShowDiffJsonExecuteNode.class);

    }

}
