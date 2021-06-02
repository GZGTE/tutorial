ui = dashboardPage(
  
  header = dashboardHeader(), 
  sidebar = dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  body = dashboardBody(
    uiOutput("tabItms")
  )
)

