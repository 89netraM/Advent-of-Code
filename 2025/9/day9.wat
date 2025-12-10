(module
  (import "wasi_snapshot_preview1" "path_open"
    (func $path_open
      (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
      (result i32)
    )
  )
  (import "wasi_snapshot_preview1" "fd_close"
    (func $fd_close
      (param i32)
      (result i32)
    )
  )
  (import "wasi_snapshot_preview1" "fd_filestat_get"
    (func $fd_filestat_get
      (param i32 i32)
      (result i32)
    )
  )
  (import "wasi_snapshot_preview1" "fd_read"
    (func $fd_read
      (param i32 i32 i32 i32)
      (result i32)
    )
  )
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write
      (param i32 i32 i32 i32)
      (result i32)
    )
  )

  (memory 1)
  (export "memory" (memory 0))

  (data (i32.const 0) "./input.txt")

  (func (export "_start")
    (local $input i32)
    (local $coordinates i32)

    (local.set $input (call $read_input))

    (local.set $coordinates (call $parse (local.get $input)))
    (call $print (call $part1 (local.get $coordinates)))
    (call $print (call $part2 (local.get $coordinates)))
  )

  (func $read_input (export "read_input") (result i32)
    (local $fd i32)
    (local $size i32)

    (call $path_open
      (i32.const 3)        ;; fd (preopened dir)
      (i32.const 0)        ;; dirflags
      (i32.const 0)        ;; path ptr
      (i32.const 11)       ;; path len
      (i32.const 0)        ;; oflags
      (i64.const 0)        ;; base rights
      (i64.const 0)        ;; inheriting rights
      (i32.const 0)        ;; fdflags
      (i32.const 12)       ;; out fd ptr
    )
    drop
    (local.set $fd (i32.load (i32.const 12)))

    (call $fd_filestat_get
      (local.get $fd)
      (i32.const 16)
    )
    drop
    (local.set $size (i32.load (i32.const 48)))

    (i32.store (i32.const 12) (i32.const 24))
    (i32.store (i32.const 16) (local.get $size))
    (call $fd_read
      (local.get $fd)      ;; fd file
      (i32.const 12)       ;; storage ptr
      (i32.const 1)        ;; storage len
      (i32.const 20)     ;; bytes read ptr
    )
    drop

    (call $fd_close
      (local.get $fd)
    )
    drop
    (i32.const 20)
  )

  (func $parse (param $input i32) (result i32)
    (local $length i32)
    (local $offset i32)

    (local $coordinates_count i32)
    (local $read i64)
    (local $number i64)

    (local.set $length (i32.load (local.get $input)))
    (local.set $input (i32.add (local.get $input) (i32.const 4)))

    (local.set $offset (i32.const 0))
    (loop $line_loop
      (local.set $number (i64.const 0))
      (loop $x_loop
        (local.set $read (i64.load8_u (i32.add (local.get $input) (local.get $offset))))
        (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
        (if
          (i32.and
            (i64.le_u
              (i64.const 48)
              (local.get $read)
            )
            (i64.le_u
              (local.get $read)
              (i64.const 57)
            )
          )
          (then
            (local.set $number (i64.add (i64.mul (local.get $number) (i64.const 10)) (i64.sub (local.get $read) (i64.const 48))))
            (br $x_loop)
          )
          (else
            (i64.store (i32.add (i32.const 7176) (i32.mul (local.get $coordinates_count) (i32.const 16))) (local.get $number))
          )
        )
      )
      (local.set $number (i64.const 0))
      (loop $y_loop
        (local.set $read (i64.load8_u (i32.add (local.get $input) (local.get $offset))))
        (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
        (if
          (i32.and
            (i64.le_u
              (i64.const 48)
              (local.get $read)
            )
            (i64.le_u
              (local.get $read)
              (i64.const 57)
            )
          )
          (then
            (local.set $number (i64.add (i64.mul (local.get $number) (i64.const 10)) (i64.sub (local.get $read) (i64.const 48))))
            (br_if $y_loop (i32.lt_s (local.get $offset) (local.get $length)))
          )
          (else nop)
        )
      )
      (i64.store (i32.add (i32.const 7184) (i32.mul (local.get $coordinates_count) (i32.const 16))) (local.get $number))
      (local.set $coordinates_count (i32.add (i32.const 1) (local.get $coordinates_count)))

      (loop $trail_loop
        (if (i32.lt_s (local.get $offset) (local.get $length))
          (then
            (local.set $read (i64.load8_u (i32.add (local.get $input) (local.get $offset))))
            (if
              (i32.and
                (i64.le_u
                  (i64.const 48)
                  (local.get $read)
                )
                (i64.le_u
                  (local.get $read)
                  (i64.const 57)
                )
              )
              (then
                (br $line_loop)
              )
              (else
                (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
                (br $trail_loop)
              )
            )
          )
          (else nop)
        )
      )
    )
    (i32.store (i32.const 7168) (local.get $coordinates_count))
    (i32.const 7168)
  )

  (func $part1 (param $coordinates i32) (result i64)
    (local $coordinates_count i32)
    (local $i i32)
    (local $j i32)
    (local $x1 i64)
    (local $x2 i64)
    (local $y1 i64)
    (local $y2 i64)
    (local $temp i64)
    (local $max_area i64)
    (local.set $coordinates_count (i32.load (local.get $coordinates)))
    (local.set $coordinates (i32.add (i32.const 8) (local.get $coordinates)))

    (loop $i_loop
      (if (i32.lt_u (local.get $i) (local.get $coordinates_count))
        (then
          (local.set $j (i32.add (local.get $i) (i32.const 1)))
          (loop $j_loop
            (if (i32.lt_u (local.get $j) (local.get $coordinates_count))
              (then
                (local.set $x1 (i64.load (i32.add (local.get $coordinates) (i32.mul (local.get $i) (i32.const 16)))))
                (local.set $x2 (i64.load (i32.add (local.get $coordinates) (i32.mul (local.get $j) (i32.const 16)))))
                (local.set $y1 (i64.load (i32.add (local.get $coordinates) (i32.add (i32.mul (local.get $i) (i32.const 16)) (i32.const 8)))))
                (local.set $y2 (i64.load (i32.add (local.get $coordinates) (i32.add (i32.mul (local.get $j) (i32.const 16)) (i32.const 8)))))
                (if (i64.le_u (local.get $x1) (local.get $x2))
                  (then
                    (local.set $temp (local.get $x2))
                    (local.set $x2 (local.get $x1))
                    (local.set $x1 (local.get $temp))
                  )
                  (else nop)
                )
                (if (i64.le_u (local.get $y1) (local.get $y2))
                  (then
                    (local.set $temp (local.get $y2))
                    (local.set $y2 (local.get $y1))
                    (local.set $y1 (local.get $temp))
                  )
                  (else nop)
                )
                (local.set
                  $temp
                  (i64.mul
                    (i64.add (i64.sub (local.get $x1) (local.get $x2)) (i64.const 1))
                    (i64.add (i64.sub (local.get $y1) (local.get $y2)) (i64.const 1))
                  )
                )
                (if (i64.le_u (local.get $max_area) (local.get $temp))
                  (then
                    (local.set $max_area (local.get $temp))
                  )
                  (else nop)
                )
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
              (else nop)
            )
          )
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $i_loop)
        )
        (else nop)
      )
    )

    (local.get $max_area)
  )

  (func $part2 (param $coordinates i32) (result i64)
    (local $coordinates_count i32)
    (local $i i32)
    (local $j i32)
    (local $k i32)
    (local $x1 i64)
    (local $x2 i64)
    (local $x3 i64)
    (local $x4 i64)
    (local $y1 i64)
    (local $y2 i64)
    (local $y3 i64)
    (local $y4 i64)
    (local $temp i64)
    (local $area i64)
    (local $max_area i64)
    (local.set $coordinates_count (i32.load (local.get $coordinates)))
    (local.set $coordinates (i32.add (i32.const 8) (local.get $coordinates)))

    (loop $i_loop
      (if (i32.lt_u (local.get $i) (local.get $coordinates_count))
        (then
          (local.set $j (i32.add (local.get $i) (i32.const 1)))
          (loop $j_loop
            (if (i32.lt_u (local.get $j) (local.get $coordinates_count))
              (then
                (local.set $x1 (i64.load (i32.add (local.get $coordinates) (i32.mul (local.get $i) (i32.const 16)))))
                (local.set $x2 (i64.load (i32.add (local.get $coordinates) (i32.mul (local.get $j) (i32.const 16)))))
                (local.set $y1 (i64.load (i32.add (local.get $coordinates) (i32.add (i32.mul (local.get $i) (i32.const 16)) (i32.const 8)))))
                (local.set $y2 (i64.load (i32.add (local.get $coordinates) (i32.add (i32.mul (local.get $j) (i32.const 16)) (i32.const 8)))))
                (if (i64.le_u (local.get $x1) (local.get $x2))
                  (then
                    (local.set $temp (local.get $x2))
                    (local.set $x2 (local.get $x1))
                    (local.set $x1 (local.get $temp))
                  )
                  (else nop)
                )
                (if (i64.le_u (local.get $y1) (local.get $y2))
                  (then
                    (local.set $temp (local.get $y2))
                    (local.set $y2 (local.get $y1))
                    (local.set $y1 (local.get $temp))
                  )
                  (else nop)
                )
                (local.set
                  $area
                  (i64.mul
                    (i64.add (i64.sub (local.get $x1) (local.get $x2)) (i64.const 1))
                    (i64.add (i64.sub (local.get $y1) (local.get $y2)) (i64.const 1))
                  )
                )
                (if (i64.le_u (local.get $max_area) (local.get $area))
                  (then
                    (block $update_max_block
                      (local.set $k (i32.const 0))
                      (loop $k_loop
                        (local.set $x3 (i64.load (i32.add (local.get $coordinates) (i32.mul (local.get $k) (i32.const 16)))))
                        (local.set $x4 (i64.load (i32.add (local.get $coordinates) (i32.mul (i32.rem_u (i32.add (local.get $k) (i32.const 1)) (local.get $coordinates_count)) (i32.const 16)))))
                        (local.set $y3 (i64.load (i32.add (local.get $coordinates) (i32.add (i32.mul (local.get $k) (i32.const 16)) (i32.const 8)))))
                        (local.set $y4 (i64.load (i32.add (local.get $coordinates) (i32.add (i32.mul (i32.rem_u (i32.add (local.get $k) (i32.const 1)) (local.get $coordinates_count)) (i32.const 16)) (i32.const 8)))))

                        (br_if $update_max_block
                          (i32.or
                            (i32.and
                              (i32.and
                                (i64.lt_u (local.get $x2) (local.get $x3))
                                (i64.lt_u (local.get $x3) (local.get $x1))
                              )
                              (i32.and
                                (i64.lt_u (local.get $y2) (local.get $y3))
                                (i64.lt_u (local.get $y3) (local.get $y1))
                              )
                            )
                            (i32.or
                              (i32.and
                                (i32.and
                                  (i64.eq (local.get $x3) (local.get $x4))
                                  (i32.and
                                    (i64.lt_u (local.get $x2) (local.get $x3))
                                    (i64.lt_u (local.get $x3) (local.get $x1))
                                  )
                                )
                                (i32.or
                                  (i32.and
                                    (i64.le_u (local.get $y3) (local.get $y2))
                                    (i64.le_u (local.get $y1) (local.get $y4))
                                  )
                                  (i32.and
                                    (i64.le_u (local.get $y4) (local.get $y2))
                                    (i64.le_u (local.get $y1) (local.get $y3))
                                  )
                                )
                              )
                              (i32.and
                                (i32.and
                                  (i64.eq (local.get $y3) (local.get $y4))
                                  (i32.and
                                    (i64.lt_u (local.get $y2) (local.get $y3))
                                    (i64.lt_u (local.get $y3) (local.get $y1))
                                  )
                                )
                                (i32.or
                                  (i32.and
                                    (i64.le_u (local.get $x3) (local.get $x2))
                                    (i64.le_u (local.get $x1) (local.get $x4))
                                  )
                                  (i32.and
                                    (i64.le_u (local.get $x4) (local.get $x2))
                                    (i64.le_u (local.get $x1) (local.get $x3))
                                  )
                                )
                              )
                            )
                          )
                        )

                        (local.set $k (i32.add (local.get $k) (i32.const 1)))
                        (br_if $k_loop (i32.lt_u (local.get $k) (local.get $coordinates_count)))
                      )
                      (local.set $max_area (local.get $area))
                    )
                  )
                  (else nop)
                )
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
              (else nop)
            )
          )
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $i_loop)
        )
        (else nop)
      )
    )

    (local.get $max_area)
  )

  (func $print (param $value i64)
    (local $length i32)

    (i32.store8 (i32.const 257) (i32.const 13))
    (i32.store8 (i32.const 258) (i32.const 10))

    (if (i64.eq (local.get $value) (i64.const 0))
      (then
        (i32.store8 (i32.const 256) (i32.const 48))
        (local.set $length (i32.const 1))
      )
      (else
        (loop $digits
          (if (i64.eq (local.get $value) (i64.const 0))
            (then nop)
            (else
              (i64.store8
                (i32.sub (i32.const 256) (local.get $length))
                (i64.add (i64.rem_u (local.get $value) (i64.const 10)) (i64.const 48))
              )
              (local.set $value (i64.div_u (local.get $value) (i64.const 10)))
              (local.set $length (i32.add (local.get $length) (i32.const 1)))
              (br $digits)
            )
          )
        )
      )
    )

    (i32.store (i32.const 12) (i32.sub (i32.const 257) (local.get $length)))
    (i32.store (i32.const 16) (i32.add (local.get $length) (i32.const 2)))
    (call $fd_write
      (i32.const 1)
      (i32.const 12)
      (i32.const 1)
      (i32.const 20)
    )
    drop
  )
)
