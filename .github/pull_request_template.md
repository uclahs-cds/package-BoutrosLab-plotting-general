## Description

<!--- Briefly describe the changes included in this pull request  --->

### Closes #... <!-- edit if this PR closes an Issue -->

## Checklist

<!--- Please read each of the following items and confirm by replacing the [ ] with a [X] --->

-   [ ] This PR does NOT contain [PHI](https://ohrpp.research.ucla.edu/hipaa/) or germline genetic data. A repo may need to be deleted if such data is uploaded. Disclosing PHI is a [major problem](https://healthitsecurity.com/news/ucla-health-reaches-7.5m-settlement-over-2015-breach-of-4.5m).

-   [ ] This PR does NOT contain molecular files, compressed files, output files such as images (*e.g.* `.png`, .`jpeg`), `.pdf`, `.RData`, `.xlsx`, `.doc`, `.ppt`, or other non-plain-text files. To automatically exclude such files using a [.gitignore](https://docs.github.com/en/get-started/getting-started-with-git/ignoring-files) file, see [here](https://github.com/uclahs-cds/template-base/blob/main/.gitignore) for example.

-   [ ] I have read the [code review guidelines](https://confluence.mednet.ucla.edu/display/BOUTROSLAB/Code+Review+Guidelines) and the [code review best practice on GitHub check-list](https://confluence.mednet.ucla.edu/display/BOUTROSLAB/Code+Review+Best+Practice+on+GitHub+-+Check+List).

-   [ ] I have set up or verified the `main` branch protection rule following the [github standards](https://confluence.mednet.ucla.edu/pages/viewpage.action?spaceKey=BOUTROSLAB&title=GitHub+Standards#GitHubStandards-Branchprotectionrule) before opening this pull request.

-   [ ] The name of the branch is meaningful and well formatted following the [standards](https://confluence.mednet.ucla.edu/display/BOUTROSLAB/Code+Review+Best+Practice+on+GitHub+-+Check+List), using [AD_username (or 5 letters of AD if AD is too long)]-[brief_description_of_branch].

-   [ ] I have added the major changes included in this pull request to the `NEWS` under the next release version or unreleased, and updated the date. I have also updated the version number in `DESCRIPTION` according to [semantic versioning](https://semver.org/) rules.

-   [ ] Both `R CMD build` and `R CMD check` run successfully.

## Testing Results

<!-- Include a small working example and a screenshot of the results if applicable. You can use the `reprex` package to automatically generate example code -->

### Case 1

``` r
# input code
```

### Case 2

``` r
# input code
```
