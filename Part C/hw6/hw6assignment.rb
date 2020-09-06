# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [
                    [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                    [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                    rotations([[0, 0], [1, 0], [-1, 0], [0, -1], [-1, -1]]), #extra 1
                    [
                      [[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], 
                      [[0, 0], [0, 1], [0, -1], [0, 2], [0, -2]]], # extra 2
                    rotations([[0, 0], [0, -1], [1, 0]])
                  ]
  # your enhancements here
  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  def self.next_cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @score = 0
    @game = game
    @delay = 500
    @current_block = MyPiece.next_piece(self)
    @is_cheated = false
  end
  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length() - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  # gets the next piece
  def next_piece
    if @is_cheated && @score >= 100
      @current_block = MyPiece.next_cheat_piece(self)
      @score -= 100
    else
      @current_block = MyPiece.next_piece(self)
    end
    @is_cheated = false
    @current_pos = nil
  end
  # try to cheat
  def cheat
    @is_cheated = true
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings
    super
    @root.bind('c', proc {@board.cheat})
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
  end
end