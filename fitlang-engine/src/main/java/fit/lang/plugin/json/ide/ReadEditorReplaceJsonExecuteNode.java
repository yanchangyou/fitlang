package fit.lang.plugin.json.ide;

/**
 * 执行节点
 */
public class ReadEditorReplaceJsonExecuteNode extends ReadComponentJsonExecuteNode {

    @Override
    String getContent() {
        return UserIdeManager.getUserIdeInterface().readEditorReplaceContent();
    }

}
