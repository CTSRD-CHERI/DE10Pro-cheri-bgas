load_package flow
project_open [lindex $quartus(args) 0]
set original_revision [get_current_revision]
foreach revision [get_project_revisions] {
    if {[string match "Seed*" $revision]} {
        set_current_revision $revision
        execute_flow -compile
    }
}
set_current_revision $original_revision
project_close
