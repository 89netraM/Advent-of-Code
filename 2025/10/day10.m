function input = get_input
  input = strsplit(strtrim(fileread("input.txt")), "\r\n")
endfunction

function button_presses = solve(line)
  [lights, line] = strtok(line, " ")
  target = lights(2:end-1) == "#"

  [buttons, line] = strtok(line, "{")
  buttons = strsplit(strtrim(buttons), " ")
  buttonIndices = cellfun(@str2double, regexp(buttons, '\d+', 'match'), 'UniformOutput', false)
  maxIndex = max(cellfun(@max, buttonIndices)) + 1
  n = numel(buttons)
  equations = zeros(n, maxIndex)
  for i = 1:n
    equations(i, buttonIndices{i} + 1) = -1
  end
  equations = [transpose(equations) eye(maxIndex) * 2]

  func = [ones(1, n) zeros(1, maxIndex)]

  solution = linprog(func, equations, target) %, zeros(1, n + maxIndex), [], repmat("S", 1, maxIndex), repmat("I", 1, n + maxIndex))

  button_presses = sum(solution(1:n))
endfunction

input = get_input()
part1 = 0
for i = 1:numel(input)
  part1 += solve(input{1,i})
end

disp(["Part 1: " num2str(part1)])
