// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.intellij.json.JsonBundle;
import com.intellij.openapi.util.text.StringUtil;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonArrayValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonObjectValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonPropertyAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.EnumObjectValueWrapper;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

public class EnumValidation implements JsonSchemaValidation {
  public static final EnumValidation INSTANCE = new EnumValidation();
  @Override
  public void validate(fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter propValue,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaType schemaType,
                       JsonValidationHost consumer,
                       fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options) {
    List<Object> enumItems = schema.getEnum();
    if (enumItems == null) return;
    final fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker walker = fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker.getWalker(propValue.getDelegate(), schema);
    if (walker == null) return;
    final String text = StringUtil.notNullize(walker.getNodeTextForValidation(propValue.getDelegate()));
    BiFunction<String, String, Boolean> eq = options.isCaseInsensitiveEnumCheck() || schema.isForceCaseInsensitive()
                                             ? String::equalsIgnoreCase
                                             : String::equals;
    for (Object object : enumItems) {
      if (checkEnumValue(object, walker, propValue, text, eq)) return;
    }
    consumer.error(JsonBundle.message("schema.validation.enum.mismatch", StringUtil.join(enumItems, o -> o.toString(), ", ")), propValue.getDelegate(),
                   fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.NonEnumValue, null, JsonErrorPriority.MEDIUM_PRIORITY);
  }

  private static boolean checkEnumValue(@NotNull Object object,
                                        @NotNull JsonLikePsiWalker walker,
                                        @Nullable fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter adapter,
                                        @NotNull String text,
                                        @NotNull BiFunction<String, String, Boolean> stringEq) {
    if (adapter != null && !adapter.shouldCheckAsValue()) return true;
    if (object instanceof fit.jetbrains.jsonSchema.impl.EnumArrayValueWrapper) {
      if (adapter instanceof fit.jetbrains.jsonSchema.extension.adapters.JsonArrayValueAdapter) {
        List<fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter> elements = ((JsonArrayValueAdapter)adapter).getElements();
        Object[] values = ((fit.jetbrains.jsonSchema.impl.EnumArrayValueWrapper)object).getValues();
        if (elements.size() == values.length) {
          for (int i = 0; i < values.length; i++) {
            if (!checkEnumValue(values[i], walker, elements.get(i), walker.getNodeTextForValidation(elements.get(i).getDelegate()), stringEq)) return false;
          }
          return true;
        }
      }
    }
    else if (object instanceof fit.jetbrains.jsonSchema.impl.EnumObjectValueWrapper) {
      if (adapter instanceof fit.jetbrains.jsonSchema.extension.adapters.JsonObjectValueAdapter) {
        List<fit.jetbrains.jsonSchema.extension.adapters.JsonPropertyAdapter> props = ((JsonObjectValueAdapter)adapter).getPropertyList();
        Map<String, Object> values = ((EnumObjectValueWrapper)object).getValues();
        if (props.size() == values.size()) {
          for (JsonPropertyAdapter prop : props) {
            if (!values.containsKey(prop.getName())) return false;
            for (JsonValueAdapter value : prop.getValues()) {
              if (!checkEnumValue(values.get(prop.getName()), walker, value, walker.getNodeTextForValidation(value.getDelegate()), stringEq)) return false;
            }
          }

          return true;
        }
      }
    }
    else {
      if (!walker.allowsSingleQuotes()) {
        if (stringEq.apply(object.toString(), text)) return true;
      }
      else {
        if (equalsIgnoreQuotes(object.toString(), text, walker.requiresValueQuotes(), stringEq)) return true;
      }
    }

    return false;
  }

  private static boolean equalsIgnoreQuotes(@NotNull final String s1,
                                            @NotNull final String s2,
                                            boolean requireQuotedValues,
                                            BiFunction<String, String, Boolean> eq) {
    final boolean quoted1 = StringUtil.isQuotedString(s1);
    final boolean quoted2 = StringUtil.isQuotedString(s2);
    if (requireQuotedValues && quoted1 != quoted2) return false;
    if (requireQuotedValues && !quoted1) return eq.apply(s1, s2);
    return eq.apply(StringUtil.unquoteString(s1), StringUtil.unquoteString(s2));
  }
}
