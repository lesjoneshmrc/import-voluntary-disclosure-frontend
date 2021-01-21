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

package forms

import forms.mappings.Mappings
import models.BoxValues
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.i18n.Messages

import javax.inject.Inject

class BoxValuesFormProvider @Inject()() extends Mappings {

  def apply(boxNumber: String)(implicit messages: Messages): Form[BoxValues] = mappings(boxNumber)

  private def mappings(boxNumber: String)(implicit messages: Messages): Form[BoxValues] = boxNumber match {
    case "22" => monetaryValuesForm(boxNumber)
    case "23" => regexValuesForm("[0-9]{6}[a-z|A-Z]", boxNumber)
    case _ => ???
  }

  private def monetaryValuesForm(boxNumber: String)(implicit messages: Messages): Form[BoxValues] =
    Form[BoxValues](
      mapping(
        "original" -> numeric(
          isCurrency = true,
          requiredKey = s"boxNumber$boxNumber.error.originalNonEmpty",
          nonNumericKey = s"boxNumber$boxNumber.error.originalNonNumber",
          invalidNumeric = s"boxNumber$boxNumber.error.originalNonNumber"
        ).verifying(
          messages(s"boxNumber$boxNumber.error.originalUpperLimit"),
          fields => fields < BigDecimal(10000000000.00)
        ).transform[String](x => x.toString, x => BigDecimal(x)),
        "amended" -> numeric(
          isCurrency = true,
          requiredKey = s"boxNumber$boxNumber.error.amendedNonEmpty",
          nonNumericKey = s"boxNumber$boxNumber.error.amendedNonNumber",
          invalidNumeric = s"boxNumber$boxNumber.error.originalNonNumber"
        ).transform[String](x => x.toString, x => BigDecimal(x))
      )(BoxValues.apply)(BoxValues.unapply)
    )

  private def regexValuesForm(pattern: String, boxNumber: String)(implicit messages: Messages): Form[BoxValues] =
    Form[BoxValues](
      mapping(
        "original" -> text(s"boxNumber$boxNumber.error.missing")
          .verifying(regexp(pattern, s"boxNumber$boxNumber.error.format")),
        "amended" -> text(s"boxNumber$boxNumber.error.missing")
          .verifying(regexp(pattern, s"boxNumber$boxNumber.error.format"))
      )(BoxValues.apply)(BoxValues.unapply)
    )
}
