/*
 * Copyright 2000-2014 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fit.intellij.json.codeinsight;

import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonObject;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonStringLiteral;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.intellij.patterns.PlatformPatterns.psiElement;

/**
 * @author Mikhail Golubev
 */
public class JsonCompletionContributor extends CompletionContributor {

    private static final PsiElementPattern.Capture<PsiElement> AFTER_COLON_IN_PROPERTY = psiElement()
            .afterLeaf(":").withSuperParent(2, JsonProperty.class)
            .andNot(psiElement().withParent(JsonStringLiteral.class));

    private static final PsiElementPattern.Capture<PsiElement> AFTER_COMMA_OR_BRACKET_IN_ARRAY = psiElement()
            .afterLeaf("[", ",").withSuperParent(2, JsonArray.class).andNot(psiElement().withParent(JsonStringLiteral.class));


    private static final PsiElementPattern.Capture<PsiElement> UNI_KEY_WORD_NO_QUOTE = psiElement()
            .afterLeaf("{");


    public JsonCompletionContributor() {

        extend(CompletionType.BASIC, AFTER_COLON_IN_PROPERTY, MyKeywordsCompletionProvider.INSTANCE);
        extend(CompletionType.BASIC, AFTER_COMMA_OR_BRACKET_IN_ARRAY, MyKeywordsCompletionProvider.INSTANCE);
        extend(CompletionType.BASIC, UNI_KEY_WORD_NO_QUOTE, LoadUniCompletionProviderNoQuote.INSTANCE);

        addUniProperty();
    }

    private void addUniProperty() {
        Map<String, JSONObject> uniMap = loadTemplateMap();

        PsiElementPattern.Capture<PsiElement> itemProperty = psiElement();

        extend(CompletionType.BASIC, itemProperty, new CompletionProvider<>() {
            @Override
            protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {

                PsiElement psiElement = parameters.getPosition();
                List<String> existedPropertyList = parseExistedPropertyList(psiElement);

                String uni = existedPropertyList.get(0);

                if (uni != null) {
                    JSONObject template = uniMap.get(uni);
                    for (Map.Entry<String, Object> item : template.entrySet()) {
                        if (existedPropertyList.contains(item.getKey())) {
                            continue;
                        }
                        String text = "\"" + item.getKey() + "\": ";
                        Object value = item.getValue();
                        if (value instanceof String) {
                            text += "\"" + value + "\"";
                        } else {
                            text += value;
                        }

                        if (!isLastProperty(psiElement)) {
                            text += ",";
                        }
                        result.addElement(LookupElementBuilder.create(text).bold());
                    }
                }
            }
        });
    }

    List<String> parseExistedPropertyList(PsiElement psiElement) {
        List<String> existedPropertyList = new ArrayList<>();
        String uni = null;
        if (psiElement.getParent() != null
                && psiElement.getParent().getParent() != null
                && psiElement.getParent().getParent().getParent() != null
                && psiElement.getParent().getParent().getParent() instanceof JsonObject) {
            PsiElement parent3 = psiElement.getParent().getParent().getParent();
            PsiElement[] children = parent3.getChildren();
            for (PsiElement child : children) {
                if (child instanceof JsonProperty) {
                    String propertyKey = child.getFirstChild().getText().replace("\"", "");
                    existedPropertyList.add(propertyKey);
                    if ("uni".equals(propertyKey)) {
                        uni = child.getLastChild().getText().replace("\"", "");
                    }
                }
            }
        }
        existedPropertyList.add(0, uni);
        return existedPropertyList;
    }

    /**
     * 是否最后一个属性，用于决定是否添加逗号
     *
     * @param psiElement
     * @return
     */
    boolean isLastProperty(PsiElement psiElement) {
        PsiElement parent2 = psiElement.getParent().getParent();
        if (parent2.getNextSibling() != null && parent2.getNextSibling().getNextSibling() != null) {
            return parent2.getNextSibling().getNextSibling().getText().equals("}");
        }
        return false;
    }

    private static class MyKeywordsCompletionProvider extends CompletionProvider<CompletionParameters> {
        private static final MyKeywordsCompletionProvider INSTANCE = new MyKeywordsCompletionProvider();
        private static final String[] KEYWORDS = new String[]{"null", "true", "false"};

        @Override
        protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
            for (String keyword : KEYWORDS) {
                result.addElement(LookupElementBuilder.create(keyword));
            }
        }
    }

    private static class LoadUniCompletionProviderNoQuote extends CompletionProvider<CompletionParameters> {
        private static final LoadUniCompletionProviderNoQuote INSTANCE = new LoadUniCompletionProviderNoQuote();

        @Override
        protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
            List<String> templates = loadTemplate();
            for (String template : templates) {
                result.addElement(LookupElementBuilder.create(template));
            }
        }
    }

    private static Map<String, JSONObject> loadTemplateMap() {
        JSONObject template = loadTemplateFromFile();
        JSONArray templateList = template.getJSONArray("template");
        Map<String, JSONObject> uniMap = new LinkedHashMap<>();
        for (Object item : templateList) {
            if (item instanceof JSONObject) {
                String uni = ((JSONObject) item).getString("uni");
                if (uni != null) {
                    uniMap.put(uni, (JSONObject) item);
                }
            }
        }
        return uniMap;
    }

    private static List<String> loadTemplate() {
        JSONObject template = loadTemplateFromFile();
        JSONArray templateList = template.getJSONArray("template");
        List<String> templates = new ArrayList<>(templateList.size());
        for (Object item : templateList) {
            if (item instanceof JSONObject) {
                JSONObject itemObject = (JSONObject) item;
                String text = itemObject.toJSONString();
                text = text.substring(1, text.length() - 1);
                text = text.replace("\":\"", "\": \"");
                if (text.startsWith("\"debug\"") || text.startsWith("\"outputRawField\"")) {
                    text += ",";
                }
                templates.add(text);
            }
        }
        return templates;
    }

    @Nullable
    private static JSONObject loadTemplateFromFile() {
        String templateJsonText = IoUtil.readUtf8(JsonCompletionContributor.class.getClassLoader().getResourceAsStream("fitTemplate.json"));
        JSONObject template = JSONObject.parseObject(templateJsonText);
        return template;
    }

    public void fillCompletionVariants(@NotNull final CompletionParameters parameters, @NotNull CompletionResultSet result) {

        result = result.withPrefixMatcher(new PrefixMatcher(result.getPrefixMatcher().getPrefix()) {
            @Override
            public boolean prefixMatches(@NotNull String name) {
                return true;
            }

            @Override
            public @NotNull PrefixMatcher cloneWithPrefix(@NotNull String prefix) {
                return new PlainPrefixMatcher(prefix);
            }
        });
        super.fillCompletionVariants(parameters, result);
    }
}
