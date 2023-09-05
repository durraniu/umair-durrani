using ParquetFiles, DataFrames, Tidier

df = DataFrame(load("data/df_raw.parquet"))

df = @chain df begin
      @clean_names
     end

names(df)

df = @chain df begin
         @rename(vehicle_id = vehicle_i_d,
                 frame_id = frame_i_d,
                 lane_id = lane_i_d)
     end

     
df = leftjoin(
      df,
      df[:, [:frame_id, :vehicle_id, :local_y, :v_length, :v_width, :v_class, :v_vel, :v_acc]],
      on = [:frame_id => :frame_id, :preceding => :vehicle_id],
      makeunique = true, 
      renamecols = "" => "_preceding"
  )


df = @chain df begin
      @rename(preceding_local_y = local_y_preceding,
              preceding_length =  v_length_preceding,
              preceding_width = v_width_preceding,
              preceding_class = v_class_preceding,
              preceding_vel = v_vel_preceding,
              preceding_acc = v_acc_preceding)
  end




  dfoo = @chain df begin
         @select(!starts_with("global")) # Keeping the desired columns only
      end
         @mutate(across(
            (starts_with("local"), starts_with("v_"), space_headway, starts_with("preceding"), -preceding, -preceding_class, -v_class),
            (x -> round.(x * 0.3048, digits=2))) # Convert to metric units
            )  
      end
         @mutate(across(
            (vehicle_id, v_class, lane_id, preceding, preceding_class), 
            (x -> string.(x)))) # Convert a few columns to factor type
      end










using DataFrames
using Random
using Tidier

# Define column names
columns = ["vehicle_id", "total_frames", "global_time", "local_x", "local_y", "global_x",
            "global_y", "v_length", "v_width", "v_class", "v_vel", "v_acc", "lane_id",
            "preceding", "following", "space_headway", "time_headway", "preceding_local_y",
            "preceding_length", "preceding_width", "preceding_class", "preceding_vel", "preceding_acc"]

# Generate random values for the row
random_values = [rand() for _ in columns]

# Create a dictionary with column names as keys and random values as values
data_dict = Dict(zip(columns, random_values))

# Create the dataframe
df = DataFrame([data_dict])

println(names(df))

dfoo = @chain df begin
        @select(!starts_with("global")) # Keeping the desired columns only
      end

println(names(dfoo))

dfoo = @chain df begin
        @select(!(starts_with("global"), total_frames)) # Keeping the desired columns only
      end



dfoo = @chain df begin
      @select(!starts_with("global"), !total_frames) # Keeping the desired columns only
      end

println(names(dfoo))


dfoo = @chain df begin
      @select(-starts_with("global"), -total_frames) # Keeping the desired columns only
      end

println(names(dfoo))