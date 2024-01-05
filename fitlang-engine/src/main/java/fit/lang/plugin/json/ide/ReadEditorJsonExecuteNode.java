package fit.lang.plugin.json.ide;

/**
 * 执行节点
 */
public class ReadEditorJsonExecuteNode extends ReadComponentJsonExecuteNode {

    @Override
    String getContent() {
        return UserIdeManager.getUserIdeInterface().readEditorContent();
    }

}
