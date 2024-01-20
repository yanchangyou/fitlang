// Copyright 2000-2021 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import com.intellij.codeInspection.util.InspectionMessage;
import com.intellij.ide.nls.NlsMessages;
import fit.intellij.json.JsonBundle;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.MultiMap;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.impl.validations.IfThenElseValidation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public final class JsonSchemaAnnotatorChecker implements fit.jetbrains.jsonSchema.extension.JsonValidationHost {
  private static final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaType> PRIMITIVE_TYPES =
    ContainerUtil.set(fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer, fit.jetbrains.jsonSchema.impl.JsonSchemaType._number, fit.jetbrains.jsonSchema.impl.JsonSchemaType._boolean, fit.jetbrains.jsonSchema.impl.JsonSchemaType._string, fit.jetbrains.jsonSchema.impl.JsonSchemaType._null);
  private final Map<PsiElement, fit.jetbrains.jsonSchema.impl.JsonValidationError> myErrors;
  @NotNull private final Project myProject;
  @NotNull private final JsonComplianceCheckerOptions myOptions;
  private boolean myHadTypeError;

  JsonSchemaAnnotatorChecker(@NotNull Project project, @NotNull JsonComplianceCheckerOptions options) {
    myProject = project;
    myOptions = options;
    myErrors = new HashMap<>();
  }

  public Map<PsiElement, fit.jetbrains.jsonSchema.impl.JsonValidationError> getErrors() {
    return myErrors;
  }

  public boolean isHadTypeError() {
    return myHadTypeError;
  }

  public static JsonSchemaAnnotatorChecker checkByMatchResult(@NotNull Project project,
                                                              @NotNull JsonValueAdapter elementToCheck,
                                                              @NotNull final fit.jetbrains.jsonSchema.impl.MatchResult result,
                                                              @NotNull JsonComplianceCheckerOptions options) {
    final List<JsonSchemaAnnotatorChecker> checkers = new ArrayList<>();
    if (result.myExcludingSchemas.isEmpty() && result.mySchemas.size() == 1) {
      final JsonSchemaAnnotatorChecker checker = new JsonSchemaAnnotatorChecker(project, options);
      checker.checkByScheme(elementToCheck, result.mySchemas.iterator().next());
      checkers.add(checker);
    }
    else {
      if (!result.mySchemas.isEmpty()) {
        checkers.add(processSchemasVariants(project, result.mySchemas, elementToCheck, false, options).getSecond());
      }
      if (!result.myExcludingSchemas.isEmpty()) {
        // we can have several oneOf groups, each about, for instance, a part of properties
        // - then we should allow properties from neighbour schemas (even if additionalProperties=false)
        final List<JsonSchemaAnnotatorChecker> list =
          ContainerUtil.map(result.myExcludingSchemas, group -> processSchemasVariants(project, group, elementToCheck, true, options).getSecond());
        checkers.add(mergeErrors(project, list, options, result.myExcludingSchemas));
      }
    }
    if (checkers.isEmpty()) return null;
    if (checkers.size() == 1) return checkers.get(0);

    return checkers.stream()
      .filter(checker -> !checker.isHadTypeError())
      .findFirst()
      .orElse(checkers.get(0));
  }

  private static JsonSchemaAnnotatorChecker mergeErrors(@NotNull Project project,
                                                        @NotNull List<JsonSchemaAnnotatorChecker> list,
                                                        @NotNull JsonComplianceCheckerOptions options,
                                                        @NotNull List<Collection<? extends fit.jetbrains.jsonSchema.impl.JsonSchemaObject>> excludingSchemas) {
    final JsonSchemaAnnotatorChecker checker = new JsonSchemaAnnotatorChecker(project, options);

    for (JsonSchemaAnnotatorChecker ch: list) {
      for (Map.Entry<PsiElement, fit.jetbrains.jsonSchema.impl.JsonValidationError> element: ch.myErrors.entrySet()) {
        fit.jetbrains.jsonSchema.impl.JsonValidationError error = element.getValue();
        if (error.getFixableIssueKind() == fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.ProhibitedProperty) {
          String propertyName = ((fit.jetbrains.jsonSchema.impl.JsonValidationError.ProhibitedPropertyIssueData)error.getIssueData()).propertyName;
          boolean skip = false;
          for (Collection<? extends fit.jetbrains.jsonSchema.impl.JsonSchemaObject> objects : excludingSchemas) {
            Set<String> keys = objects.stream()
              .filter(o -> !o.hasOwnExtraPropertyProhibition())
              .map(o -> o.getProperties().keySet()).flatMap(Set::stream).collect(Collectors.toSet());
            if (keys.contains(propertyName)) skip = true;
          }
          if (skip) continue;
        }
        checker.myErrors.put(element.getKey(), error);
      }
    }
    return checker;
  }

  @Override
  public void error(@InspectionMessage String error, final PsiElement holder,
                    JsonErrorPriority priority) {
    error(error, holder, fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.None, null, priority);
  }

  @Override
  public void error(final PsiElement newHolder, fit.jetbrains.jsonSchema.impl.JsonValidationError error) {
    error(error.getMessage(), newHolder, error.getFixableIssueKind(), error.getIssueData(), error.getPriority());
  }

  @Override
  public void error(@InspectionMessage String error, final PsiElement holder,
                    fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind fixableIssueKind,
                    fit.jetbrains.jsonSchema.impl.JsonValidationError.IssueData data,
                    JsonErrorPriority priority) {
    if (myErrors.containsKey(holder)) return;
    myErrors.put(holder, new fit.jetbrains.jsonSchema.impl.JsonValidationError(error, fixableIssueKind, data, priority));
  }

  @Override
  public void typeError(final @NotNull PsiElement value, @Nullable fit.jetbrains.jsonSchema.impl.JsonSchemaType currentType, final fit.jetbrains.jsonSchema.impl.JsonSchemaType @NotNull ... allowedTypes) {
    if (allowedTypes.length == 0) return;
    String currentTypeDesc = currentType == null ? "" : (" " + JsonBundle.message("schema.validation.actual") + currentType.getName() + ".");
    String prefix = JsonBundle.message("schema.validation.incompatible.types") + "\n";
    if (allowedTypes.length == 1) {
      error(prefix + " " + JsonBundle.message("schema.validation.required.one", allowedTypes[0].getName(), currentTypeDesc), value,
            fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.ProhibitedType,
            new fit.jetbrains.jsonSchema.impl.JsonValidationError.TypeMismatchIssueData(allowedTypes),
            JsonErrorPriority.TYPE_MISMATCH);
    } else {
      final String typesText = Arrays.stream(allowedTypes)
                                     .map(fit.jetbrains.jsonSchema.impl.JsonSchemaType::getName)
                                     .distinct()
                                     .sorted(Comparator.naturalOrder())
                                     .collect(Collectors.joining(", "));
      error(prefix + " " + JsonBundle.message("schema.validation.required.one.of", typesText, currentTypeDesc), value,
            fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.ProhibitedType,
            new fit.jetbrains.jsonSchema.impl.JsonValidationError.TypeMismatchIssueData(allowedTypes),
            JsonErrorPriority.TYPE_MISMATCH);
    }
    myHadTypeError = true;
  }

  @Override
  public fit.jetbrains.jsonSchema.impl.MatchResult resolve(fit.jetbrains.jsonSchema.impl.JsonSchemaObject schemaObject) {
    return new fit.jetbrains.jsonSchema.impl.JsonSchemaResolver(myProject, schemaObject).detailedResolve();
  }

  @Override
  public @Nullable fit.jetbrains.jsonSchema.extension.JsonValidationHost checkByMatchResult(JsonValueAdapter adapter,
                                                                                            MatchResult result,
                                                                                            JsonComplianceCheckerOptions options) {
    return checkByMatchResult(myProject, adapter, result, options);
  }

  @Override
  public boolean isValid() {
    return myErrors.size() == 0 && !myHadTypeError;
  }

  private static Collection<fit.jetbrains.jsonSchema.extension.JsonSchemaValidation> getAllValidations(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                                                                                       fit.jetbrains.jsonSchema.impl.JsonSchemaType type,
                                                                                                       @NotNull JsonValueAdapter value) {
    Set<fit.jetbrains.jsonSchema.extension.JsonSchemaValidation> validations = new LinkedHashSet<>();
    validations.add(fit.jetbrains.jsonSchema.impl.validations.EnumValidation.INSTANCE);
    if (type != null) {
      validations.add(fit.jetbrains.jsonSchema.impl.validations.TypeValidation.INSTANCE);
      if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._string_number.equals(type)) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.NumericValidation.INSTANCE);
        validations.add(fit.jetbrains.jsonSchema.impl.validations.StringValidation.INSTANCE);
      }
      else if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._number.equals(type) || fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer.equals(type)) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.NumericValidation.INSTANCE);
      }
      else if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._string.equals(type)) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.StringValidation.INSTANCE);
      }
      else if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._array.equals(type)) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.ArrayValidation.INSTANCE);
      }
      else if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._object.equals(type)) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.ObjectValidation.INSTANCE);
      }
    }
    if (!value.isShouldBeIgnored()) {
      if (schema.hasNumericChecks() && value.isNumberLiteral()) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.NumericValidation.INSTANCE);
      }
      if (schema.hasStringChecks() && value.isStringLiteral()) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.StringValidation.INSTANCE);
      }
      if (schema.hasArrayChecks() && value.isArray()) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.ArrayValidation.INSTANCE);
      }
      if (hasMinMaxLengthChecks(schema)) {
        if (value.isStringLiteral()) {
          validations.add(fit.jetbrains.jsonSchema.impl.validations.StringValidation.INSTANCE);
        }
        else if (value.isArray()) {
          validations.add(fit.jetbrains.jsonSchema.impl.validations.ArrayValidation.INSTANCE);
        }
      }
      if (schema.hasObjectChecks() && value.isObject()) {
        validations.add(fit.jetbrains.jsonSchema.impl.validations.ObjectValidation.INSTANCE);
      }
    }
    if (schema.getNot() != null) {
      validations.add(fit.jetbrains.jsonSchema.impl.validations.NotValidation.INSTANCE);
    }
    if (schema.getIfThenElse() != null) {
      validations.add(IfThenElseValidation.INSTANCE);
    }
    return validations;
  }

  public void checkByScheme(@NotNull JsonValueAdapter value, @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaType type = fit.jetbrains.jsonSchema.impl.JsonSchemaType.getType(value);
    for (JsonSchemaValidation validation : getAllValidations(schema, type, value)) {
      validation.validate(value, schema, type, this, myOptions);
    }
  }

  @Override
  public void checkObjectBySchemaRecordErrors(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema, @NotNull JsonValueAdapter object) {
    final JsonSchemaAnnotatorChecker checker = checkByMatchResult(myProject, object, new JsonSchemaResolver(myProject, schema).detailedResolve(), myOptions);
    if (checker != null) {
      myHadTypeError = checker.isHadTypeError();
      myErrors.putAll(checker.getErrors());
    }
  }

  @Override
  public void addErrorsFrom(JsonValidationHost otherHost) {
    this.myErrors.putAll(((JsonSchemaAnnotatorChecker)otherHost).myErrors);
  }

  @NotNull
  private static Pair<fit.jetbrains.jsonSchema.impl.JsonSchemaObject, JsonSchemaAnnotatorChecker> processSchemasVariants(
    @NotNull Project project, @NotNull final Collection<? extends fit.jetbrains.jsonSchema.impl.JsonSchemaObject> collection,
    @NotNull final JsonValueAdapter value, boolean isOneOf, JsonComplianceCheckerOptions options) {

    final JsonSchemaAnnotatorChecker checker = new JsonSchemaAnnotatorChecker(project, options);
    final fit.jetbrains.jsonSchema.impl.JsonSchemaType type = fit.jetbrains.jsonSchema.impl.JsonSchemaType.getType(value);
    fit.jetbrains.jsonSchema.impl.JsonSchemaObject selected = null;
    if (type == null) {
      if (!value.isShouldBeIgnored()) checker.typeError(value.getDelegate(), null, getExpectedTypes(collection));
    }
    else {
      final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> filtered = new ArrayList<>(collection.size());
      fit.jetbrains.jsonSchema.impl.JsonSchemaType altType = value.getAlternateType(type);
      for (fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema: collection) {
        if (!areSchemaTypesCompatible(schema, type)
            && !areSchemaTypesCompatible(schema, altType)) continue;
        filtered.add(schema);
      }
      if (filtered.isEmpty()) {
        checker.typeError(value.getDelegate(), altType, getExpectedTypes(collection));
      }
      else if (filtered.size() == 1) {
        selected = filtered.get(0);
        checker.checkByScheme(value, selected);
      }
      else {
        if (isOneOf) {
          selected = checker.processOneOf(value, filtered);
        }
        else {
          selected = checker.processAnyOf(value, filtered);
        }
      }
    }
    return Pair.create(selected, checker);
  }

  private final static fit.jetbrains.jsonSchema.impl.JsonSchemaType[] NO_TYPES = new fit.jetbrains.jsonSchema.impl.JsonSchemaType[0];
  public static fit.jetbrains.jsonSchema.impl.JsonSchemaType[] getExpectedTypes(final Collection<? extends fit.jetbrains.jsonSchema.impl.JsonSchemaObject> schemas) {
    final List<fit.jetbrains.jsonSchema.impl.JsonSchemaType> list = new ArrayList<>();
    for (fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema : schemas) {
      final fit.jetbrains.jsonSchema.impl.JsonSchemaType type = schema.getType();
      if (type != null) {
        list.add(type);
      } else {
        final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaType> variants = schema.getTypeVariants();
        if (variants != null) {
          list.addAll(variants);
        }
      }
    }
    return list.isEmpty() ? NO_TYPES : list.toArray(NO_TYPES);
  }

  public static boolean areSchemaTypesCompatible(@NotNull final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema, @NotNull final fit.jetbrains.jsonSchema.impl.JsonSchemaType type) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaType matchingSchemaType = getMatchingSchemaType(schema, type);
    if (matchingSchemaType != null) return matchingSchemaType.equals(type);
    if (schema.getEnum() != null) {
      return PRIMITIVE_TYPES.contains(type);
    }
    return true;
  }

  @Nullable
  public static fit.jetbrains.jsonSchema.impl.JsonSchemaType getMatchingSchemaType(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema, @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaType input) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaType matchType = schema.getType();
    if (matchType != null) {
      if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer.equals(input) && fit.jetbrains.jsonSchema.impl.JsonSchemaType._number.equals(matchType)) {
        return input;
      }
      if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._string_number.equals(input) && (fit.jetbrains.jsonSchema.impl.JsonSchemaType._number.equals(matchType)
                                                          || fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer.equals(matchType)
                                                          || fit.jetbrains.jsonSchema.impl.JsonSchemaType._string.equals(matchType))) {
        return input;
      }
      return matchType;
    }
    if (schema.getTypeVariants() != null) {
      Set<fit.jetbrains.jsonSchema.impl.JsonSchemaType> matchTypes = schema.getTypeVariants();
      if (matchTypes.contains(input)) {
        return input;
      }
      if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer.equals(input) && matchTypes.contains(fit.jetbrains.jsonSchema.impl.JsonSchemaType._number)) {
        return input;
      }
      if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._string_number.equals(input) &&
          (matchTypes.contains(fit.jetbrains.jsonSchema.impl.JsonSchemaType._number)
           || matchTypes.contains(fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer)
           || matchTypes.contains(fit.jetbrains.jsonSchema.impl.JsonSchemaType._string))) {
        return input;
      }
      //nothing matches, lets return one of the list so that other heuristics does not match
      return matchTypes.iterator().next();
    }
    if (!schema.getProperties().isEmpty() && fit.jetbrains.jsonSchema.impl.JsonSchemaType._object.equals(input)) return fit.jetbrains.jsonSchema.impl.JsonSchemaType._object;
    return null;
  }



  private static boolean hasMinMaxLengthChecks(fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema) {
    return schema.getMinLength() != null || schema.getMaxLength() != null;
  }

  @Nullable
  public static String getValue(PsiElement propValue, fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema) {
    final JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(propValue, schema);
    assert walker != null;
    JsonValueAdapter adapter = walker.createValueAdapter(propValue);
    if (adapter != null && !adapter.shouldCheckAsValue()) return null;
    return walker.getNodeTextForValidation(propValue);
  }

  // returns the schema, selected for annotation
  private fit.jetbrains.jsonSchema.impl.JsonSchemaObject processOneOf(@NotNull JsonValueAdapter value, List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> oneOf) {
    final List<JsonSchemaAnnotatorChecker> candidateErroneousCheckers = new ArrayList<>();
    final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> candidateErroneousSchemas = new ArrayList<>();
    final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> correct = new SmartList<>();
    for (fit.jetbrains.jsonSchema.impl.JsonSchemaObject object : oneOf) {
      // skip it if something JS awaited, we do not process it currently
      if (object.isShouldValidateAgainstJSType()) continue;

      final JsonSchemaAnnotatorChecker checker = new JsonSchemaAnnotatorChecker(myProject, myOptions);
      checker.checkByScheme(value, object);

      if (checker.isCorrect()) {
        candidateErroneousCheckers.clear();
        candidateErroneousSchemas.clear();
        correct.add(object);
      }
      else {
        candidateErroneousCheckers.add(checker);
        candidateErroneousSchemas.add(object);
      }
    }
    if (correct.size() == 1) return correct.get(0);
    if (correct.size() > 0) {
      final fit.jetbrains.jsonSchema.impl.JsonSchemaType type = fit.jetbrains.jsonSchema.impl.JsonSchemaType.getType(value);
      if (type != null) {
        // also check maybe some currently not checked properties like format are different with schemes
        // todo note that JsonSchemaObject#equals is broken by design, so normally it shouldn't be used until rewritten
        //  but for now we use it here to avoid similar schemas being marked as duplicates
        if (new HashSet<>(correct).size() > 1 && !schemesDifferWithNotCheckedProperties(correct)) {
          error(JsonBundle.message("schema.validation.to.more.than.one"), value.getDelegate(), JsonErrorPriority.MEDIUM_PRIORITY);
        }
      }
      return ContainerUtil.getLastItem(correct);
    }

    return showErrorsAndGetLeastErroneous(candidateErroneousCheckers, candidateErroneousSchemas, true);
  }

  private static boolean schemesDifferWithNotCheckedProperties(@NotNull final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> list) {
    return list.stream().anyMatch(s -> !StringUtil.isEmptyOrSpaces(s.getFormat()));
  }

  private enum AverageFailureAmount {
    Light,
    MissingItems,
    Medium,
    Hard,
    NotSchema
  }

  @NotNull
  private static AverageFailureAmount getAverageFailureAmount(@NotNull JsonSchemaAnnotatorChecker checker) {
    int lowPriorityCount = 0;
    boolean hasMedium = false;
    boolean hasMissing = false;
    boolean hasHard = false;
    Collection<fit.jetbrains.jsonSchema.impl.JsonValidationError> values = checker.getErrors().values();
    for (fit.jetbrains.jsonSchema.impl.JsonValidationError value: values) {
      switch (value.getPriority()) {
        case LOW_PRIORITY -> lowPriorityCount++;
        case MISSING_PROPS -> hasMissing = true;
        case MEDIUM_PRIORITY -> hasMedium = true;
        case TYPE_MISMATCH -> hasHard = true;
        case NOT_SCHEMA -> {
          return AverageFailureAmount.NotSchema;
        }
      }
    }

    if (hasHard) {
      return AverageFailureAmount.Hard;
    }

    // missing props should win against other conditions
    if (hasMissing) {
      return AverageFailureAmount.MissingItems;
    }

    if (hasMedium) {
      return AverageFailureAmount.Medium;
    }

    return lowPriorityCount <= 3 ? AverageFailureAmount.Light : AverageFailureAmount.Medium;
  }

  // returns the schema, selected for annotation
  private fit.jetbrains.jsonSchema.impl.JsonSchemaObject processAnyOf(@NotNull JsonValueAdapter value, List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> anyOf) {
    final List<JsonSchemaAnnotatorChecker> candidateErroneousCheckers = new ArrayList<>();
    final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> candidateErroneousSchemas = new ArrayList<>();

    for (fit.jetbrains.jsonSchema.impl.JsonSchemaObject object : anyOf) {
      final JsonSchemaAnnotatorChecker checker = new JsonSchemaAnnotatorChecker(myProject, myOptions);
      checker.checkByScheme(value, object);
      if (checker.isCorrect()) {
        return object;
      }
      // maybe we still find the correct schema - continue to iterate
      candidateErroneousCheckers.add(checker);
      candidateErroneousSchemas.add(object);
    }

    return showErrorsAndGetLeastErroneous(candidateErroneousCheckers, candidateErroneousSchemas, false);
  }

  /**
   * Filters schema validation results to get the result with the "minimal" amount of errors.
   * This is needed in case of oneOf or anyOf conditions, when there exist no match.
   * I.e., when we have multiple schema candidates, but none is applicable.
   * In this case we need to show the most "suitable" error messages
   *   - by detecting the most "likely" schema corresponding to the current entity
   */
  @Nullable
  private fit.jetbrains.jsonSchema.impl.JsonSchemaObject showErrorsAndGetLeastErroneous(@NotNull List<JsonSchemaAnnotatorChecker> candidateErroneousCheckers,
                                                                                        @NotNull List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> candidateErroneousSchemas,
                                                                                        boolean isOneOf) {
    fit.jetbrains.jsonSchema.impl.JsonSchemaObject current = null;
    JsonSchemaObject currentWithMinAverage = null;
    Optional<AverageFailureAmount> minAverage = candidateErroneousCheckers.stream()
                                                                          .map(c -> getAverageFailureAmount(c))
                                                                          .min(Comparator.comparingInt(c -> c.ordinal()));
    int min = minAverage.orElse(AverageFailureAmount.Hard).ordinal();

    int minErrorCount = candidateErroneousCheckers.stream().map(c -> c.getErrors().size()).min(Integer::compareTo).orElse(Integer.MAX_VALUE);

    MultiMap<PsiElement, fit.jetbrains.jsonSchema.impl.JsonValidationError> errorsWithMinAverage = new MultiMap<>();
    MultiMap<PsiElement, fit.jetbrains.jsonSchema.impl.JsonValidationError> allErrors = new MultiMap<>();
    for (int i = 0; i < candidateErroneousCheckers.size(); i++) {
      JsonSchemaAnnotatorChecker checker = candidateErroneousCheckers.get(i);
      final boolean isMoreThanMinErrors = checker.getErrors().size() > minErrorCount;
      final boolean isMoreThanAverage = getAverageFailureAmount(checker).ordinal() > min;
      if (!isMoreThanMinErrors) {
        if (isMoreThanAverage) {
          currentWithMinAverage = candidateErroneousSchemas.get(i);
        }
        else {
          current = candidateErroneousSchemas.get(i);
        }

        for (Map.Entry<PsiElement, fit.jetbrains.jsonSchema.impl.JsonValidationError> entry: checker.getErrors().entrySet()) {
          (isMoreThanAverage ? errorsWithMinAverage : allErrors).putValue(entry.getKey(), entry.getValue());
        }
      }
    }

    if (allErrors.isEmpty()) allErrors = errorsWithMinAverage;

    for (Map.Entry<PsiElement, Collection<fit.jetbrains.jsonSchema.impl.JsonValidationError>> entry : allErrors.entrySet()) {
      Collection<fit.jetbrains.jsonSchema.impl.JsonValidationError> value = entry.getValue();
      if (value.size() == 0) continue;
      if (value.size() == 1) {
        error(entry.getKey(), value.iterator().next());
        continue;
      }
      fit.jetbrains.jsonSchema.impl.JsonValidationError error = tryMergeErrors(value, isOneOf);
      if (error != null) {
        error(entry.getKey(), error);
      }
      else {
        for (fit.jetbrains.jsonSchema.impl.JsonValidationError validationError : value) {
          error(entry.getKey(), validationError);
        }
      }
    }

    if (current == null) {
      current = currentWithMinAverage;
    }
    if (current == null) {
      current = ContainerUtil.getLastItem(candidateErroneousSchemas);
    }

    return current;
  }

  @Nullable
  private static fit.jetbrains.jsonSchema.impl.JsonValidationError tryMergeErrors(@NotNull Collection<fit.jetbrains.jsonSchema.impl.JsonValidationError> errors, boolean isOneOf) {
    fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind commonIssueKind = null;
    for (fit.jetbrains.jsonSchema.impl.JsonValidationError error : errors) {
      fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind currentIssueKind = error.getFixableIssueKind();
      if (currentIssueKind == fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.None) return null;
      else if (commonIssueKind == null) commonIssueKind = currentIssueKind;
      else if (currentIssueKind != commonIssueKind) return null;
    }

    if (commonIssueKind == fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.NonEnumValue) {
      String prefix = JsonBundle.message("schema.validation.enum.mismatch", "");
      @NlsSafe String text = errors.stream()
        // todo remove this ugly textual cutting
        .map(e -> StringUtil.trimEnd(StringUtil.trimStart(e.getMessage(), prefix), prefix) /*ltr and rtl*/)
        .map(e -> StringUtil.split(e, ", "))
        .flatMap(e -> e.stream())
        .distinct()
        .collect(Collectors.joining(", "));
      return new fit.jetbrains.jsonSchema.impl.JsonValidationError(prefix + text, commonIssueKind, null, errors.iterator().next().getPriority());
    }

    if (commonIssueKind == fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.MissingProperty) {
      String sets = errors.stream().map(e -> (fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingMultiplePropsIssueData)e.getIssueData())
        .map(d -> d.getMessage(false)).collect(NlsMessages.joiningOr());
      return new fit.jetbrains.jsonSchema.impl.JsonValidationError(JsonBundle.message(
        isOneOf ? "schema.validation.one.of.property.sets.required" : "schema.validation.at.least.one.of.property.sets.required", sets),
                                     isOneOf ? fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.MissingOneOfProperty : fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.MissingAnyOfProperty,
                                     new fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingOneOfPropsIssueData(
                                       ContainerUtil.map(errors, e -> (fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingMultiplePropsIssueData)e.getIssueData())), errors.iterator().next().getPriority());
    }

    if (commonIssueKind == fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.ProhibitedType) {
      final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaType> allTypes = errors.stream().map(e -> (fit.jetbrains.jsonSchema.impl.JsonValidationError.TypeMismatchIssueData)e.getIssueData())
        .flatMap(d -> Arrays.stream(d.expectedTypes)).collect(Collectors.toSet());

      if (allTypes.size() == 1) return errors.iterator().next();

      List<String> actualInfos = errors.stream().map(e -> e.getMessage()).map(JsonSchemaAnnotatorChecker::fetchActual).distinct().toList();
      String actualInfo = actualInfos.size() == 1 ? (" " + JsonBundle.message("schema.validation.actual") + actualInfos.get(0) + ".") : "";
      String commonTypeMessage = JsonBundle.message("schema.validation.incompatible.types") + "\n" +
                                 JsonBundle.message("schema.validation.required.one.of",
                                                    allTypes.stream().map(t -> t.getDescription()).sorted().collect(Collectors.joining(", ")),
                                                    actualInfo);
      return new fit.jetbrains.jsonSchema.impl.JsonValidationError(commonTypeMessage, fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.TypeMismatch,
                                     new JsonValidationError.TypeMismatchIssueData(ContainerUtil.toArray(allTypes, JsonSchemaType[]::new)),
                                     errors.iterator().next().getPriority());
    }

    return null;
  }

  private static String fetchActual(String message) {
    String actualMessage = JsonBundle.message("schema.validation.actual");
    int actual = message.indexOf(actualMessage);
    if (actual == -1) return null;
    String substring = message.endsWith(actualMessage) ? message.substring(0, actual) : message.substring(actual + actualMessage.length());
    return StringUtil.trimEnd(substring, ".");
  }

  public boolean isCorrect() {
    return myErrors.isEmpty();
  }
}
