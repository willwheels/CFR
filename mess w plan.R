library(projmgr)

cfr_plan_txt <- "
- title: 2009 is a problem
  description: >
    The 2009 files continue to display weird results
  issue:
    - title: Investigate issue
      body: Not sure about path
      assignees: [willwheels]
      labels: [long-term]


- title: Redo network
  description: >
    Recreate network with tidygraph; do all data steps in initial dataframe
  issue:
    - title: Remove igraph steps
      body: nuke igraph
    - title: Move data steps
      body: Move all weight and size calculations to data frame creation
    - title: Visualizations
      body: recreate network visualizations"

cfr_plan <- read_plan(cfr_plan_txt)

cfr_repo_ref <- create_repo_ref("willwheels", "CFR")

check_credentials(cfr_repo_ref)

cfr_plan

cfr_repo_ref

post_plan(cfr_repo_ref, cfr_plan)
