package fit.lang.plugin.json.ide.node;

/**
 * 执行节点
 */
public class ReadEditorJsonExecuteNode extends ReadComponentJsonExecuteNode {

    @Override
    String getContent() {
        return UserIdeManager.getUserIdeInterface().readEditorContent();
    }

}
