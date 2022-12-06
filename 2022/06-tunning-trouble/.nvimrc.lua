local function find_terminal_chan_in_tab(tab)
  local windows = vim.api.nvim_tabpage_list_wins(tab or 0)
  for _, window in ipairs(windows) do
    local buf = vim.api.nvim_win_get_buf(window)
    local chan = vim.api.nvim_buf_get_option(buf, "channel")
    local chan_info = vim.api.nvim_get_chan_info(chan)
    if chan_info.mode == "terminal" then
      return merge(chan_info, { window = window })
    end
  end
end

local augroup = vim.api.nvim_create_augroup("AdventOfCode", { clear = true })

local function reeval_on_write(opts)
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = augroup,
    buffer = opts.buffer,
    callback = function()
      vim.api.nvim_chan_send(opts.channel, ":r\nprocess test\n")
    end,
  })
  print "Attached BufWrite handler to re-evaluated expression 'process test'"
end

local function attach_ghci_command(buf)
  local name = vim.api.nvim_buf_get_name(buf)
  vim.api.nvim_buf_create_user_command(buf, "Ghci", function()
    local existing_chan = find_terminal_chan_in_tab()
    if not existing_chan then
      print "We have to create a new window with terminal"
    elseif #existing_chan.argv > 2 and existing_chan.argv[3] == ("ghci " .. name) then
      reeval_on_write { buffer = buf, channel = existing_chan.id }
    else
      vim.api.nvim_chan_send(existing_chan.id, "ghci " .. name .. "\n")
      reeval_on_write { buffer = buf, channel = existing_chan.id }
    end
  end, {})
end

vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = "*.hs",
  group = augroup,
  callback = function(opts)
    attach_ghci_command(opts.buf)
  end,
})

local function is_haskell(buf)
  local name = vim.api.nvim_buf_get_name(buf)
  return string.match(name, "hs$")
end

for _, buf in ipairs(vim.api.nvim_list_bufs()) do
  if is_haskell(buf) then
    attach_ghci_command(buf)
  end
end

vim.defer_fn(function ()
  if is_haskell(0) then
    vim.cmd "bo vs term://$SHELL"
  end
end, 200)
