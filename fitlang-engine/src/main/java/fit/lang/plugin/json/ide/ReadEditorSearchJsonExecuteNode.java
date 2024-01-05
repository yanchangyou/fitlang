package fit.lang.plugin.json.ide;

/**
 * 执行节点
 */
public class ReadEditorSearchJsonExecuteNode extends ReadComponentJsonExecuteNode {

    @Override
    String getContent() {
        return UserIdeManager.getUserIdeInterface().readEditorSearchContent();
    }

}
