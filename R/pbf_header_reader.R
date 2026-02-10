#' Read Bounding Box from OSM PBF Header
#'
#' Extracts the bbox from the HeaderBlock of an OSM PBF file using pure R.
#' This reads only the first header blob (OSMHeader), not the data blocks.
#'
#' @param pbf_file Path to an .osm.pbf file
#' @return Named list with `center` (c(lng, lat)) and `bbox` (c(lng_min, lat_min, lng_max, lat_max))
#'   or NULL if bbox not found or file invalid
#' @keywords internal
read_pbf_header_bbox <- function(pbf_file) {
  if (!file.exists(pbf_file)) {
    return(NULL)
  }

  con <- NULL
  tryCatch(
    {
      con <- file(pbf_file, "rb")

      # Read BlobHeader length (4 bytes, big endian)
      header_len <- readBin(con, "integer", n = 1, size = 4, endian = "big")
      if (header_len <= 0 || header_len > 65536) {
        return(NULL)
      } # Sanity check

      # Read BlobHeader
      header_bytes <- readBin(con, "raw", n = header_len)

      # Parse BlobHeader to get datasize
      pos <- 1
      datasize <- NULL

      while (pos <= length(header_bytes)) {
        tag <- as.integer(header_bytes[pos])
        field_num <- bitwShiftR(tag, 3)
        wire_type <- bitwAnd(tag, 7)
        pos <- pos + 1

        if (field_num == 1 && wire_type == 2) {
          # type string - skip
          vi <- .read_varint(header_bytes, pos)
          pos <- vi$next_pos + vi$value
        } else if (field_num == 2 && wire_type == 2) {
          # indexdata - skip
          vi <- .read_varint(header_bytes, pos)
          pos <- vi$next_pos + vi$value
        } else if (field_num == 3 && wire_type == 0) {
          # datasize
          datasize <- .read_varint(header_bytes, pos)$value
          break
        } else {
          break
        }
      }

      if (is.null(datasize) || datasize <= 0 || datasize > 33554432) {
        return(NULL)
      }

      # Read Blob
      blob_bytes <- readBin(con, "raw", n = datasize)
      close(con)
      con <- NULL

      # Parse Blob to get zlib_data
      pos <- 1
      zlib_data <- NULL

      while (pos <= length(blob_bytes)) {
        if (pos > length(blob_bytes)) {
          break
        }
        tag <- as.integer(blob_bytes[pos])
        field_num <- bitwShiftR(tag, 3)
        wire_type <- bitwAnd(tag, 7)
        pos <- pos + 1

        if (field_num == 1 && wire_type == 2) {
          # raw - skip
          vi <- .read_varint(blob_bytes, pos)
          pos <- vi$next_pos + vi$value
        } else if (field_num == 2 && wire_type == 0) {
          # raw_size - skip
          pos <- .read_varint(blob_bytes, pos)$next_pos
        } else if (field_num == 3 && wire_type == 2) {
          # zlib_data - this is what we want
          len <- .read_varint(blob_bytes, pos)
          pos <- len$next_pos
          zlib_data <- blob_bytes[pos:(pos + len$value - 1)]
          break
        } else {
          break
        }
      }

      if (is.null(zlib_data)) {
        return(NULL)
      }

      # Decompress zlib data
      decompressed <- tryCatch(
        {
          .zlib_decompress(zlib_data)
        },
        error = function(e) NULL
      )

      if (is.null(decompressed)) {
        return(NULL)
      }

      # Parse HeaderBlock to get bbox
      pos <- 1
      bbox <- NULL

      while (pos <= length(decompressed)) {
        if (pos > length(decompressed)) {
          break
        }
        tag <- as.integer(decompressed[pos])
        field_num <- bitwShiftR(tag, 3)
        wire_type <- bitwAnd(tag, 7)
        pos <- pos + 1

        if (field_num == 1 && wire_type == 2) {
          # bbox message
          vi <- .read_varint(decompressed, pos)
          len <- vi$value
          pos <- vi$next_pos
          bbox <- .parse_header_bbox(decompressed[pos:(pos + len - 1)])
          break
        } else if (wire_type == 2) {
          # Skip other string/bytes fields
          vi <- .read_varint(decompressed, pos)
          pos <- vi$next_pos + vi$value
        } else if (wire_type == 0) {
          # Skip varint fields
          pos <- .read_varint(decompressed, pos)$next_pos
        } else {
          break
        }
      }

      return(bbox)
    },
    error = function(e) {
      NULL
    },
    finally = {
      if (!is.null(con)) try(close(con), silent = TRUE)
    }
  )
}

#' @keywords internal
.read_varint <- function(bytes, start = 1) {
  result <- 0
  shift <- 0
  pos <- start

  while (pos <= length(bytes)) {
    byte <- as.integer(bytes[pos])
    result <- result + bitwAnd(byte, 127) * (2^shift)
    if (bitwAnd(byte, 128) == 0) {
      break
    }
    shift <- shift + 7
    pos <- pos + 1
  }

  list(value = result, next_pos = pos + 1)
}

#' @keywords internal
.parse_header_bbox <- function(bbox_bytes) {
  pos <- 1
  coords <- list(left = NULL, right = NULL, top = NULL, bottom = NULL)

  while (pos <= length(bbox_bytes)) {
    tag <- as.integer(bbox_bytes[pos])
    field_num <- bitwShiftR(tag, 3)
    wire_type <- bitwAnd(tag, 7)
    pos <- pos + 1

    if (wire_type == 0) {
      # varint
      vi <- .read_varint(bbox_bytes, pos)
      pos <- vi$next_pos
      # Zigzag decode using arithmetic to handle 64-bit nanodegree values
      decoded <- .zigzag_decode_64(as.numeric(vi$value))

      if (field_num == 1) {
        coords$left <- decoded
      } else if (field_num == 2) {
        coords$right <- decoded
      } else if (field_num == 3) {
        coords$top <- decoded
      } else if (field_num == 4) {
        coords$bottom <- decoded
      }
    } else {
      break
    }
  }

  if (!any(sapply(coords, is.null)) && !any(sapply(coords, is.na))) {
    NANO <- 1e-9
    lng_min <- as.numeric(coords$left) * NANO
    lng_max <- as.numeric(coords$right) * NANO
    lat_min <- as.numeric(coords$bottom) * NANO
    lat_max <- as.numeric(coords$top) * NANO

    list(
      center = c(mean(c(lng_min, lng_max)), mean(c(lat_min, lat_max))),
      bbox = c(lng_min, lat_min, lng_max, lat_max)
    )
  } else {
    NULL
  }
}

#' @keywords internal
.zigzag_decode_64 <- function(n) {
  if (n %% 2 == 0) {
    n / 2
  } else {
    -(n + 1) / 2
  }
}

#' @keywords internal
.zlib_decompress <- function(data) {
  if (length(data) == 0) {
    stop("Empty data to decompress")
  }

  # Method 1: Use zlib-flate if available
  zf <- Sys.which("zlib-flate")
  if (nzchar(zf)) {
    temp_in <- tempfile()
    temp_out <- tempfile()
    writeBin(data, temp_in)
    on.exit(unlink(c(temp_in, temp_out)), add = TRUE)

    # Use system2 for better cross-platform support
    # Redirect both stdout and stderr to avoid issues
    exit_code <- system2(
      zf,
      args = c("-uncompress"),
      stdin = temp_in,
      stdout = temp_out,
      stderr = FALSE
    )

    if (exit_code == 0 && file.exists(temp_out) && file.size(temp_out) > 0) {
      return(readBin(temp_out, "raw", n = file.size(temp_out)))
    }
  }

  # Method 2: Try Python zlib
  py <- Sys.which("python3")
  if (nzchar(py)) {
    temp_in <- tempfile()
    temp_out <- tempfile()
    py_script <- tempfile(fileext = ".py")
    on.exit(unlink(c(temp_in, temp_out, py_script)), add = TRUE)

    cat(
      "import sys, zlib; open(sys.argv[1],'wb').write(zlib.decompress(open(sys.argv[2],'rb').read()))",
      file = py_script
    )
    writeBin(data, temp_in)

    exit_code <- system2(
      py,
      args = c(shQuote(py_script), shQuote(temp_out), shQuote(temp_in))
    )
    if (exit_code == 0 && file.exists(temp_out) && file.size(temp_out) > 0) {
      return(readBin(temp_out, "raw", n = file.size(temp_out)))
    }
  }

  # Method 3: Try R's memDecompress by converting zlib to gzip format
  result <- tryCatch(
    {
      if (
        length(data) > 6 &&
          data[1] == as.raw(0x78) &&
          data[2] %in% c(as.raw(0x9c), as.raw(0x01), as.raw(0xda))
      ) {
        # Strip 2-byte zlib header and 4-byte ADLER32 checksum to get raw DEFLATE
        deflate_data <- data[3:(length(data) - 4)]
        # Wrap in minimal gzip container (header + raw deflate + dummy trailer)
        gzip_header <- as.raw(c(
          0x1f,
          0x8b,
          0x08,
          0x00,
          0x00,
          0x00,
          0x00,
          0x00,
          0x00,
          0x03
        ))
        gzip_trailer <- as.raw(rep(0x00, 8))
        memDecompress(c(gzip_header, deflate_data, gzip_trailer), type = "gzip")
      } else {
        memDecompress(data, type = "gzip")
      }
    },
    error = function(e) NULL
  )
  if (!is.null(result)) {
    return(result)
  }

  stop("Unable to decompress PBF header")
}

#' Get PBF Bounding Box with Fallbacks
#'
#' Tries multiple methods to extract the bounding box from an OSM PBF file:
#' 1. Pure R header parsing (fastest, no dependencies)
#' 2. osmium CLI (if available in PATH)
#' 3. sf sampling (if sf is installed, reads first 10 features)
#'
#' @param pbf_file Path to an .osm.pbf file
#' @return Named list with `center` (c(lng, lat)) and `bbox` (c(lng_min, lat_min, lng_max, lat_max))
#'   or NULL if bbox could not be determined
#' @keywords internal
.get_pbf_extent <- function(pbf_file) {
  if (!file.exists(pbf_file)) {
    return(NULL)
  }

  # Method 1: Pure R header parsing (fastest, no external deps)
  result <- tryCatch(read_pbf_header_bbox(pbf_file), error = function(e) NULL)
  if (!is.null(result)) {
    return(result)
  }

  # Method 2: osmium fileinfo (reads header only)
  osmium_path <- Sys.which("osmium")
  if (nzchar(osmium_path)) {
    tryCatch(
      {
        info <- system2(
          osmium_path,
          args = c("fileinfo", shQuote(pbf_file)),
          stdout = TRUE,
          stderr = FALSE
        )
        bbox_line <- grep("^\\s+\\([-\\d\\.]+,", info, value = TRUE)
        if (length(bbox_line) > 0) {
          bbox_str <- sub("^\\s+\\(([^)]+)\\).*", "\\1", bbox_line[1])
          coords <- as.numeric(strsplit(bbox_str, ",")[[1]])
          if (length(coords) == 4 && !any(is.na(coords))) {
            return(list(
              center = c(mean(coords[c(1, 3)]), mean(coords[c(2, 4)])),
              bbox = coords
            ))
          }
        }
      },
      error = function(e) NULL
    )
  }

  # Method 3: sf sampling (try points first, then lines for road-only PBFs)
  if (requireNamespace("sf", quietly = TRUE)) {
    old_idx <- Sys.getenv("OSM_USE_CUSTOM_INDEXING", unset = NA)
    Sys.setenv(OSM_USE_CUSTOM_INDEXING = "NO")
    on.exit(
      {
        if (is.na(old_idx)) {
          Sys.unsetenv("OSM_USE_CUSTOM_INDEXING")
        } else {
          Sys.setenv(OSM_USE_CUSTOM_INDEXING = old_idx)
        }
      },
      add = TRUE
    )
    for (layer in c("points", "lines")) {
      tryCatch(
        {
          feats <- sf::st_read(
            pbf_file,
            query = sprintf("SELECT * FROM %s LIMIT 10", layer),
            quiet = TRUE
          )
          if (nrow(feats) > 0) {
            bbox <- sf::st_bbox(feats)
            return(list(
              center = c(
                mean(c(bbox$xmin, bbox$xmax)),
                mean(c(bbox$ymin, bbox$ymax))
              ),
              bbox = as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")])
            ))
          }
        },
        error = function(e) NULL
      )
    }
  }

  NULL
}
