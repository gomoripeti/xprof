defmodule XprofGuiLiveviewWeb.PageControllerTest do
  use XprofGuiLiveviewWeb.ConnCase

  test "GET / redirects to MonitoringLive", %{conn: conn} do
    conn = get(conn, ~p"/")
    # Should contain the XProf welcome message
    assert html_response(conn, 200) =~ "Welcome to XProf"
  end
end
