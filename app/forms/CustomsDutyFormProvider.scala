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

import config.AppConfig
import forms.mappings.Mappings
import models.UnderpaymentAmount
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.i18n.Messages

import javax.inject.Inject


class CustomsDutyFormProvider @Inject()(implicit appConfig: AppConfig) extends Mappings {

  def apply()(implicit messages: Messages): Form[UnderpaymentAmount] =
    Form(
      mapping(
        "original" -> numeric(
          isCurrency = true,
          requiredKey = "customsDuty.error.originalNonEmpty",
          invalidDecimalPlacesKey = "customsDuty.error.originalNonNumber",
          nonNumericKey = "customsDuty.error.originalNonNumber"
        ).verifying(
          messages("customsDuty.error.originalUpperLimit"),
          fields => fields < BigDecimal(10000000000.00)
        ),
        "amended" -> numeric(
          isCurrency = true,
          requiredKey = "customsDuty.error.amendedNonEmpty",
          invalidDecimalPlacesKey = "customsDuty.error.amendedNonNumber",
          nonNumericKey = "customsDuty.error.amendedNonNumber"
        )
      )(UnderpaymentAmount.apply)(UnderpaymentAmount.unapply)
    )

}
