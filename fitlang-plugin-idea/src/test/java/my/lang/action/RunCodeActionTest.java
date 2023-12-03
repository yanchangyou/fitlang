package my.lang.action;

import junit.framework.TestCase;

public class RunCodeActionTest extends TestCase {

    public void testRunLanguageFile() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/go/hello.go";
        String result = new ScriptRunCodeAction().executeCode("", filePath);
        System.out.println(result);
    }
}