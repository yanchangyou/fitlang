package my.lang.action;

import junit.framework.TestCase;

public class RunCodeActionTest extends TestCase {

    String projectPath = "";

    public void testRunGo() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/lang/go/hello.go";
        String result = new ScriptRunCodeAction().executeCode("", filePath, projectPath);
        System.out.println(result);
    }

    public void testRunJava() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/lang/java/Hello.java";
        String result = new ScriptRunCodeAction().executeCode("", filePath, projectPath);
        System.out.println(result);
    }

    public void testRunJs() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/lang/js/hello.js";
        String result = new ScriptRunCodeAction().executeCode("", filePath, projectPath);
        System.out.println(result);
    }

    public void testRunPython() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/lang/python/hello.py";
        String result = new ScriptRunCodeAction().executeCode("", filePath, projectPath);
        System.out.println(result);
    }

    public void testRunRust() {
        String filePath = "/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/lang/rust/hello.rs";
        String result = new ScriptRunCodeAction().executeCode("", filePath, projectPath);
        System.out.println(result);
    }
}