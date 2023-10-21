package my.lang.parser;

import com.intellij.json.json5.Json5ParserDefinition;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;
import my.lang.MyLanguage;
import my.lang.psi.MyLanguageFile;

/**
 * @author yanchangyou
 */
public class MyLanguageParserDefinition extends Json5ParserDefinition {

    public static final IFileElementType FILE = new IFileElementType(MyLanguage.INSTANCE);
//
//    @NotNull
//    @Override
//    public PsiElement createElement(ASTNode astNode) {
//        String text = astNode.getText();
//        System.out.println("PsiElement-astNode: " + astNode);
////        astNode.getElementType().equals(PROPERTY)
//        if ("uni".equals(text)) {
//            return new JsonBooleanLiteralImpl(astNode);
//        } else if ("\"uni\"".equals(text)) {
//            return new JsonBooleanLiteralImpl(astNode);
//        }
//        return Factory.createElement(astNode);
//    }

    @Override
    public IFileElementType getFileNodeType() {
        return FILE;
    }

    @Override
    public PsiFile createFile(FileViewProvider viewProvider) {
        return new MyLanguageFile(viewProvider);
    }

}
