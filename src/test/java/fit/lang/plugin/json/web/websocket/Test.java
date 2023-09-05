//package fit.lang.plugin.json.web.websocket;
//
//import io.netty.channel.Channel;
//import io.netty.channel.group.ChannelGroup;
//import io.netty.channel.group.DefaultChannelGroup;
//import io.netty.util.concurrent.GlobalEventExecutor;
//
//public class ChannelSupervise {
//    private static ChannelGroup globalGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE);
////    private static ConcurrentMapChannelMap =new ConcurrentHashMap();
//
//    public static void addChannel(String apiToken, Channel channel) {
//        GlobalGroup.add(channel);
//        if (null != apiToken) {
//            ChannelMap.put(apiToken, channel.id());
//        }
//    }
//
//    public static void updateChannel(String apiToken, Channel channel) {
//        Channel chan = GlobalGroup.find(channel.id());
//        if (null == chan) {
//            addChannel(apiToken, channel);
//        } else {
//            ChannelMap.put(apiToken, channel.id());
//        }
//    }
//
//    public static void removeChannel(Channel channel) {
//        GlobalGroup.remove(channel);
//        Collectionvalues = ChannelMap.values();
//        values.remove(channel.id());
//    }
//
//    public static Channel findChannel(String apiToken) {
//        ChannelId chanId = ChannelMap.get(apiToken);
//        if (null == chanId) {
//            return null;
//        }
//        return GlobalGroup.find(ChannelMap.get(apiToken));
//    }
//
//    public static void sendToAll(TextWebSocketFrame tws) {
//        GlobalGroup.writeAndFlush(tws);
//    }
//
//    public static void sendToSimple(String apiToken, TextWebSocketFrame tws) {
//        GlobalGroup.find(ChannelMap.get(apiToken)).writeAndFlush(tws);
//    }
//}