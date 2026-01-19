defmodule XprofGuiLiveviewWeb.PageController do
  use XprofGuiLiveviewWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
