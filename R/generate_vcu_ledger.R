#' Calculate and Round VCUs
#'
#' This function calculates and rounds Verified Carbon Units (VCUs) based on a set of input parameters.
#'
#' @param full_cals A data frame containing full calculations.
#' @param file_name Character string specifying the output file name for the results.
#'
#' @return A data frame containing calculated and rounded VCUs, and the results are written to an Excel file.
#'
#' @import tidyverse
#' @import data.table
#' @importFrom writexl write_xlsx
#'
#' @examples
#' # Example usage:
#' generate_vcu_ledger(full_cals = my_data, file_name = "output.xlsx")
#'
#' @export

generate_vcu_ledger <- function(full_cals, file_name) {
    input <- full_cals |>
        mutate(
            Erem_eq38_field_min_buffer = Erem_eq38_field - buffer_field, # to separate removals from emissions reduction we need to subtract the buffer from the removals only accordint to eq 66 vmd0042
            Vintage = 22,
            Continent = "EU",
            "HY Crop Name" = actual_crop_name,
            "Sub-group" = paste0(user_id, Vintage), # 22 is just a mock up for the baseline harvest year
            Standard = "Verra VM00042",
            Program = "AgreenaCarbon Europe",
            Status = "Active",
            "Management venue" = "Agreena Registry",
            "VCU matching" = NA,
            "Retirement beneficiary" = NA
        ) |>
        rename(
            "Field ID" = field_id,
            "Country" = field_def_country,
            "PAI ID" = PAI_ID,
            "User ID" = user_id,
        ) |>
        select(Standard, Vintage, Program, Status, `Management venue`, `HY Crop Name`, `VCU matching`, `Field ID`, `Country`, `PAI ID`, `User ID`, `Sub-group`, Vintage, Continent, Erem_eq38_field_min_buffer, ERR_eq39_field, Ered_eq37_field, VCU_field_eq66)

    err_separation <- input |>
        pivot_longer(
            cols = c(Erem_eq38_field_min_buffer, Ered_eq37_field),
            names_to = "Type",
            values_to = "vcu_value"
        ) |>
        mutate(Type = ifelse(Type == "Erem_eq38_field_min_buffer", "removal", "reduction"))

    premium_pool <- err_separation |>
        group_by(`Sub-group`, Type) |>
        summarise(
            premium_pool = ceiling(sum(vcu_value) * 10 / 100)
        )

    vcu_field <- err_separation |>
        mutate(
            VCU_field_id = paste("vcu_field", row_number(), sep = "_"),
            Asset = floor(vcu_value),
            VCU_remainder = vcu_value - Asset,
            Level = "Field"
        )
    vcu_pai <- vcu_field |>
        group_by(Standard, Vintage, Program, Status, `Management venue`, Country, `VCU matching`, `PAI ID`, `User ID`, `Sub-group`, Continent, Type) |>
        summarise(
            VCU_pai = sum(VCU_remainder)
        ) |>
        mutate(
            Asset = floor(VCU_pai),
            VCU_remainder = VCU_pai - Asset,
            Level = "PAI"
        )
    vcu_subgroup <- vcu_pai |>
        group_by(Standard, Vintage, Program, Status, `Management venue`, Country, `VCU matching`, `User ID`, `Sub-group`, Continent, Type) |>
        summarise(
            VCU_subgroup = sum(VCU_remainder)
        ) |>
        mutate(
            Asset = floor(VCU_subgroup),
            VCU_remainder = VCU_subgroup - Asset,
            Level = "Sub-group"
        )
    vcu_user <- vcu_subgroup |>
        group_by(Standard, Vintage, Program, Status, `Management venue`, Country, `VCU matching`, `User ID`, Continent, Type) |>
        summarise(
            VCU_user = sum(VCU_remainder)
        ) |>
        mutate(
            Asset = floor(VCU_user),
            VCU_remainder = VCU_user - Asset,
            Level = "User"
        )
    vcu_country <- vcu_user |>
        group_by(Standard, Vintage, Program, Status, `Management venue`, Country, `VCU matching`, Continent, Type) |>
        summarise(
            VCU_country = sum(VCU_remainder)
        ) |>
        mutate(
            Asset = floor(VCU_country),
            VCU_remainder = VCU_country - Asset,
            Level = "Country"
        )
    vcu_Continent <- vcu_country |>
        group_by(Standard, Vintage, Program, Status, `Management venue`, `VCU matching`, Continent, Type) |>
        summarise(
            VCU_Continent = sum(VCU_remainder)
        ) |>
        mutate(
            Asset = floor(VCU_Continent),
            VCU_remainder = VCU_Continent - Asset,
            Level = "Continent"
        )

    vcu_field <- vcu_field |>
        mutate(uncount = ifelse(Asset == 0, 1, abs(Asset))) |>
        uncount(uncount) |>
        mutate(Asset = ifelse(Asset == 0, 0, ifelse(Asset < 0, -1, 1))) # this is to make sure that the uncounting works for negative and zero numbers
    vcu_pai <- vcu_pai |>
        mutate(uncount = ifelse(Asset == 0, 1, abs(Asset))) |>
        uncount(uncount) |>
        mutate(Asset = ifelse(Asset == 0, 0, ifelse(Asset < 0, -1, 1)))
    vcu_subgroup <- vcu_subgroup |>
        mutate(uncount = ifelse(Asset == 0, 1, abs(Asset))) |>
        uncount(uncount) |>
        mutate(Asset = ifelse(Asset == 0, 0, ifelse(Asset < 0, -1, 1)))
    vcu_user <- vcu_user |>
        mutate(uncount = ifelse(Asset == 0, 1, abs(Asset))) |>
        uncount(uncount) |>
        mutate(Asset = ifelse(Asset == 0, 0, ifelse(Asset < 0, -1, 1)))
    vcu_country <- vcu_country |>
        mutate(uncount = ifelse(Asset == 0, 1, abs(Asset))) |>
        uncount(uncount) |>
        mutate(Asset = ifelse(Asset == 0, 0, ifelse(Asset < 0, -1, 1)))
    vcu_Continent <- vcu_Continent |>
        mutate(uncount = ifelse(Asset == 0, 1, abs(Asset))) |>
        uncount(uncount) |>
        mutate(Asset = ifelse(Asset == 0, 0, ifelse(Asset < 0, -1, 1)))

    out <- bind_rows(vcu_field, vcu_pai, vcu_subgroup, vcu_user, vcu_country, vcu_Continent)
    out <- out |>
        select(-VCU_field_eq66, -vcu_value, -VCU_field_id, -vcu_value, -VCU_remainder, -ERR_eq39_field, -VCU_pai, -VCU_subgroup, -VCU_user, -VCU_country, -VCU_Continent) |>
        mutate(
            `Agreena Cert. ID` = row_number(),
            `Retirement beneficiary` = NA
        ) |>
        select(`Agreena Cert. ID`, Asset, `HY Crop Name`, Level, Standard, Vintage, Program, Type, Status, `Management venue`, `VCU matching`, Continent, `Country`, `User ID`, `PAI ID`, `Sub-group`, `Field ID`, `Retirement beneficiary`)

    # Canceling out vcus from lower levels with excess vcs from higher levels

    # rebalance_vcus <- function(out, column) {
    #     switch(column,
    #         "PAI ID" = level <- "PAI",
    #         "User ID" = level <- "User",
    #         "Sub-group" = level <- "Sub-group"
    #     )
    #     for (j in c("reduction", "removal")) {
    #         negative_field_vcus <- out %>% filter(Asset < 0, Level == "Field", Type == j)
    #         positive_level_vcus <- out %>% filter(Asset > 0, Level == level, Type == j)

    #         # negative_field_vcus <- negative_field_vcus |> filter(`User ID` == 4767)

    #         for (i in 1:nrow(negative_field_vcus)) {
    #             selected_positive <- positive_level_vcus |> filter(as.vector(get(column)) == as.vector(negative_field_vcus[i, column]), Asset > 0)
    #             if (nrow(selected_positive) > 0) {
    #                 print(i)
    #                 positive_level_vcus[positive_level_vcus$`Agreena Cert. ID` %in% selected_positive[1, "Agreena Cert. ID"], "Asset"] <- 0
    #                 negative_field_vcus[i, "Asset"] <- 0
    #             }
    #         }
    #         out[match(negative_field_vcus$`Agreena Cert. ID`, out$`Agreena Cert. ID`), "Asset"] <- negative_field_vcus$Asset
    #         out[match(positive_level_vcus$`Agreena Cert. ID`, out$`Agreena Cert. ID`), "Asset"] <- positive_level_vcus$Asset
    #     }
    #     return(out)
    # }

    rebalance_vcus <- function(out, column) {
        level <- switch(column,
            "PAI ID" = "PAI",
            "User ID" = "User",
            "Sub-group" = "Sub-group"
        )

        setDT(out) # Convert to data.table for better performance

        for (j in c("reduction", "removal")) {
            negative_field_vcus <- out[Asset < 0 & Level == "Field" & Type == j]
            positive_level_vcus <- out[Asset > 0 & Level == level & Type == j]

            for (i in seq_len(nrow(negative_field_vcus))) {
                selected_positive <- which(positive_level_vcus[, get(column)] == as.vector(negative_field_vcus[i, get(column)]) & positive_level_vcus[, Asset] > 0)[1]
                if (!is.na(selected_positive) && selected_positive > 0) {
                    positive_level_vcus[selected_positive, Asset := 0]
                    negative_field_vcus[i, Asset := 0]
                }
            }
            out[match(negative_field_vcus$`Agreena Cert. ID`, out$`Agreena Cert. ID`), "Asset" := negative_field_vcus$Asset]
            out[match(positive_level_vcus$`Agreena Cert. ID`, out$`Agreena Cert. ID`), "Asset" := positive_level_vcus$Asset]
        }

        return(out)
    }

    out <- rebalance_vcus(out, "PAI ID")
    out <- rebalance_vcus(out, "Sub-group")
    out <- rebalance_vcus(out, "User ID")

    # rebalance_errs <- function(out) {
    #     for (j in c("reduction", "removal")) {
    #         negative <- out %>% filter(Asset < 0, Level == "Field", Type == j)
    #         positive <- out %>% filter(Asset > 0, Level == "Field", Type == switch(j,
    #             "reduction" = "removal",
    #             "removal" = "reduction"
    #         ))

    #         # negative_field_vcus <- negative_field_vcus |> filter(`User ID` == 4767)

    #         for (i in 1:nrow(negative)) {
    #             # selected_positive <- positive |> filter(as.vector(`Field ID`) == as.vector(negative[i, "Field ID"]), Asset > 0)
    #             selected_positive <- which(positive$`Field ID` == as.vector(negative[i, "Field ID"]) & positive$Asset > 0)[1]
    #             if (selected_positive > 0) {
    #                 print(i)
    #                 positive[selected_positive, "Asset"] <- 0
    #                 negative[i, "Asset"] <- 0
    #             } else if (is.na(selected_positive)) {
    #                 next
    #             }
    #         }
    #         out[match(negative$`Agreena Cert. ID`, out$`Agreena Cert. ID`), "Asset"] <- negative$Asset
    #         out[match(positive$`Agreena Cert. ID`, out$`Agreena Cert. ID`), "Asset"] <- positive$Asset
    #     }
    #     return(out)
    # }

    # Using data.table for better performancea
    rebalance_errs <- function(x) {
        setDT(x) # Convert to data.table

        for (j in c("reduction", "removal")) {
            negative <- x[Asset < 0 & Level == "Field" & Type == j]
            positive <- x[Asset > 0 & Level == "Field" & Type == ifelse(j == "reduction", "removal", "reduction")]

            for (i in 1:nrow(negative)) {
                selected_positive <- which(positive$`Field ID` == as.vector(negative[i, `Field ID`]) & positive$Asset > 0)[1]
                if (!is.na(selected_positive) && selected_positive > 0) {
                    positive[selected_positive, Asset := 0]
                    negative[i, Asset := 0]
                }
            }

            x[match(negative$`Agreena Cert. ID`, x$`Agreena Cert. ID`), Asset := negative$Asset]
            x[match(positive$`Agreena Cert. ID`, x$`Agreena Cert. ID`), Asset := positive$Asset]
        }
        return(x)
    }
    out <- rebalance_errs(out)

    # Takes the pre calculated premium pool and locks the VCU's that are in the first n positions of the group. The number of positions is given by the size of premium pool for that group. The premium pool is calculated as 10% of the total VCU's in the group.
    # The VCU's are locked by changing the status to "locked" in the first n positions of the group. The VCU's are sorted by descending order of the asset value. The VCU's that have a negative asset value are not locked.

    out <- as.data.frame(out)
    negative_assets <- out[out$Asset <= 0, ]
    positive_assets <- out[out$Asset > 0, ]
    positive_assets <- positive_assets %>%
        dplyr::left_join(premium_pool, by = c("Sub-group", "Type")) %>%
        group_by(`Sub-group`, Type) %>%
        group_modify(~ .x %>% mutate(Status = ifelse(row_number() <= first(.x$premium_pool) & !is.na(first(.x$premium_pool)), "locked", Status))) %>%
        ungroup()
    out <- bind_rows(positive_assets, negative_assets)
    out <- out %>%
        arrange(desc(`Field ID`), desc(`Sub-group`), desc(Type))

    # Some subgroups with a lot of fields with removals and reductions smaller than 1 will have a mismatch between premium pool and number of locked VCUs. This happens because the premium pool is larged than the available amount ot integers VCUS that can be locked. In this case the premium pool is reduced to the number of available VCUs. The solution is to remove the premium pool from the calculation and lock all VCUs before converting the asset to integers.

    check <- out %>%
        dplyr::filter(Status == "locked") %>%
        group_by(`Sub-group`, Type) %>%
        summarise(sum_locked = n()) %>%
        left_join(premium_pool, by = c("Sub-group", "Type")) %>%
        mutate(check = ifelse(sum_locked == premium_pool, "Match", "Mismatch"))  |> 
        dplyr::select(check)
    if(nrow(check[check$check == "Mismatch", ]) > 0) {
        warning(paste("There is a mismatch between the premium pool and the number of locked VCUs.", table(check$check)))
    }
    # filter out rows with zero credits
    out <- out %>% dplyr::filter(Asset != 0)
    # out <- out %>% filter(Asset == 1)

    writexl::write_xlsx(out, file_name)

    return(out)
}
