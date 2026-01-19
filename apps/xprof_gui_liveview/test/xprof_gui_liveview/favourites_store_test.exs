defmodule XprofGuiLiveview.FavouritesStoreTest do
  use ExUnit.Case, async: false

  alias XprofGuiLiveview.FavouritesStore

  setup do
    # Clear the table before each test
    :ets.delete_all_objects(:xprof_favourites)
    :ok
  end

  describe "FavouritesStore" do
    test "starts with empty favourites" do
      assert FavouritesStore.list() == []
    end

    test "can add a favourite" do
      assert :ok = FavouritesStore.add("lists:map/2")
      assert FavouritesStore.list() == ["lists:map/2"]
    end

    test "can add multiple favourites" do
      assert :ok = FavouritesStore.add("lists:map/2")
      assert :ok = FavouritesStore.add("lists:filter/2")
      assert :ok = FavouritesStore.add("maps:get/2")

      favourites = FavouritesStore.list()
      assert length(favourites) == 3
      assert "lists:map/2" in favourites
      assert "lists:filter/2" in favourites
      assert "maps:get/2" in favourites
    end

    test "newest favourites appear first" do
      :ok = FavouritesStore.add("first")
      # Small delay to ensure different timestamps
      :timer.sleep(10)
      :ok = FavouritesStore.add("second")
      :timer.sleep(10)
      :ok = FavouritesStore.add("third")

      assert FavouritesStore.list() == ["third", "second", "first"]
    end

    test "can remove a favourite" do
      :ok = FavouritesStore.add("lists:map/2")
      :ok = FavouritesStore.add("lists:filter/2")

      assert :ok = FavouritesStore.remove("lists:map/2")
      assert FavouritesStore.list() == ["lists:filter/2"]
    end

    test "removing non-existent favourite returns error" do
      assert {:error, :not_found} = FavouritesStore.remove("not_there")
    end

    test "can check if query is a favourite" do
      :ok = FavouritesStore.add("lists:map/2")

      assert FavouritesStore.member?("lists:map/2") == true
      assert FavouritesStore.member?("lists:filter/2") == false
    end

    test "adding duplicate favourite updates timestamp" do
      :ok = FavouritesStore.add("lists:map/2")
      :timer.sleep(10)
      :ok = FavouritesStore.add("lists:filter/2")
      :timer.sleep(10)

      # Add duplicate - it should move to the front
      :ok = FavouritesStore.add("lists:map/2")

      # The duplicate should now be first (most recent)
      [first | _] = FavouritesStore.list()
      assert first == "lists:map/2"
    end

    test "handles empty string gracefully" do
      # Empty strings are not valid queries (validated by when guard)
      assert_raise FunctionClauseError, fn ->
        FavouritesStore.add("")
      end
    end
  end
end
