//package fit.lang.plugin.json.web;
//
//import com.alibaba.fastjson2.JSONObject;
//import fit.lang.plugin.json.ExecuteJsonNodeUtil;
//import fit.lang.plugin.json.web.ServerJsonExecuteNode;
//import junit.framework.TestCase;
//import org.junit.Assert;
//
//public class ProxyJsonExecuteNodeTest extends TestCase {
//
//    @Override
//    protected void setUp() throws Exception {
//        String flow = "{" +//
//                "   'uni': 'server'," +
//                "   'port': 11101," +
//                "   'service': {" +
//                "       '/hello':{" +
//                "           'uni':'hello'" +
//                "       }" +
//                "   }" +
//                "}";
//        JSONObject contextParam = new JSONObject();
//        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);
//
//        System.out.println(output);
//
//    }
//
//    public void testExecute() throws InterruptedException {
//        String flow = "{" +//
//                "   'uni': 'proxy'," +
//                "   'url': 'http://127.0.0.1:11101/'" +
//                "}";
//        JSONObject contextParam = new JSONObject();
//        contextParam.put(ServerJsonExecuteNode.SERVICE_PATH, "/");
//        contextParam.put(ServerJsonExecuteNode.REQUEST_PATH, "/hello");
//        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);
//
//        System.out.println(output);
//
//        Assert.assertEquals("{\"message\":\"hello, world!\"}", output);
//    }
////
////    public void testExecute2() throws InterruptedException {
////        String flow = "{" +//
////                "   'uni': 'proxy'," +
////                "   'url': 'http://127.0.0.1:11101/'" +
////                "}";
////        JSONObject contextParam = new JSONObject();
////        contextParam.put(ServerJsonExecuteNode.REQUEST_PATH, "/static/sub/page.html");
////        contextParam.put(ServerJsonExecuteNode.SERVICE_PATH, "/static");
////        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);
////
////        System.out.println(output);
////
////        Assert.assertEquals("{\"_raw\":\"hello,sub!\\n\"}", output);
////    }
////
////    public void testExecute3() throws InterruptedException {
////        String flow = "{" +//
////                "   'uni': 'proxy'," +
////                "   'url': 'http://127.0.0.1:11101/'" +
////                "}";
////        JSONObject contextParam = new JSONObject();
////        contextParam.put(ServerJsonExecuteNode.REQUEST_PATH, "/static/sub/page.html");
////        contextParam.put(ServerJsonExecuteNode.SERVICE_PATH, "/static/");
////        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);
////
////        System.out.println(output);
////
////        Assert.assertEquals("{\"_raw\":\"hello,sub!\\n\"}", output);
////    }
//}