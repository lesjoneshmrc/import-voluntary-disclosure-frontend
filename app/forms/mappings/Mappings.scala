/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package forms.mappings

import config.AppConfig
import models.Enumerable
import play.api.data.FieldMapping
import play.api.data.Forms.of
import play.api.i18n.Messages

import java.time.LocalDate

trait Mappings extends Formatters with Constraints {

  protected def text(errorKey: String = "error.required"): FieldMapping[String] =
    of(stringFormatter(errorKey))

  protected def int(requiredKey: String = "error.required",
                    wholeNumberKey: String = "error.wholeNumber",
                    nonNumericKey: String = "error.nonNumeric"): FieldMapping[Int] =
    of(intFormatter(requiredKey, wholeNumberKey, nonNumericKey))

  protected def numeric(isCurrency: Boolean = false,
                        requiredKey: String = "error.required",
                        invalidNumeric: String = "error.invalidNumeric",
                        nonNumericKey: String = "error.nonNumeric"
                       ): FieldMapping[BigDecimal] =
    if (isCurrency) {
      of(numericFormatter(isCurrency = true, requiredKey, invalidNumeric, nonNumericKey))
    } else {
      of(numericFormatter(isCurrency = false, requiredKey, invalidNumeric, nonNumericKey))
    }

  protected def boolean(requiredKey: String = "error.required",
                        invalidKey: String = "error.boolean"): FieldMapping[Boolean] =
    of(booleanFormatter(requiredKey, invalidKey))

  protected def localDate(invalidKey: String,
                          allRequiredKey: String,
                          twoRequiredKey: String,
                          requiredKey: String,
                          dayMonthLengthKey: String = "error.date.length",
                          yearLengthKey: String = "error.year.length",
                          validatePastKey: Option[String] = None,
                          args: Seq[String] = Seq.empty)(implicit messages: Messages): FieldMapping[LocalDate] =
    of(new LocalDateFormatter(invalidKey, allRequiredKey, twoRequiredKey, requiredKey, dayMonthLengthKey, yearLengthKey, validatePastKey, args))

  protected def enumerable[A](requiredKey: String = "error.required",
                              invalidKey: String = "error.invalid")(implicit ev: Enumerable[A]): FieldMapping[A] =
    of(enumerableFormatter[A](requiredKey, invalidKey))

  protected def foreignCurrency(requiredKey: String = "error.required",
                                invalidNumeric: String = "error.invalid"): FieldMapping[String] =
    of(foreignCurrencyFormatter(requiredKey, invalidNumeric))

}
