package my.lang.action;

import junit.framework.TestCase;

public class RunCodeActionTest extends TestCase {

    public void testRunGo() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/go/hello.go";
        String result = new ScriptRunCodeAction().executeCode("", filePath);
        System.out.println(result);
    }

    public void testRunJava() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/java/Hello.java";
        String result = new ScriptRunCodeAction().executeCode("", filePath);
        System.out.println(result);
    }
}