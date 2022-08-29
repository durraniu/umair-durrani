# Load libraries---------
import numpy as np
import pandas as pd


# Load data---------------
dfn1_py = pd.read_csv("dfn1.csv")



# Gipps model--------------

def simulate_gipps_py(resolution, N, dfn1, 
    xn1, vn1, xn_first, vn_first, ln, an, 
    Vn, tau, bn_const, bcap):
  
    ####### Time #############################################

    # Last time frame of the simulation:
    last_time = (dfn1.shape[0] - 1) * resolution

    # Time vector:
    Time = list( np.arange(0, last_time+1, resolution) )

    # Length of the Time vector
    time_length = len(Time)

    list_of_N_veh = [None] * N



    for n in 1:length(list_of_N_veh)
        ####### Assign names to Lead Vehicle Parameters ##########
        
        if (n == 1) 
            
            # Lead vehicle position
            xn1 = dfn1[!, xn1]
            
            # Lead vehicle speed
            vn1 = dfn1[!, vn1]
            
        end

        ln1 = ln[n]

        ####### Allocate Vectors ##################################

        # free-flow speed
        vn_ff = Vector{Union{Float64, Missing}}(missing, time_length)
        
        # car-following speed
        vn_cf = Vector{Union{Float64, Missing}}(missing, time_length)
        
        # speed
        vn = Vector{Union{Float64, Missing}}(missing, time_length)
        

        # position
        xn = Vector{Union{Float64, Missing}}(missing, time_length)
        

        # spacing
        sn = Vector{Union{Float64, Missing}}(missing, time_length)
        

        # speed difference
        deltav = Vector{Union{Float64, Missing}}(missing, time_length)
        

        # acceleration rate
        bn = Vector{Union{Float64, Missing}}(missing, time_length)
        

            ######## Initial values for Following vehicle ##################################

        # speed
        vn_ff[1] = vn_first[n]
        vn_cf[1] = vn_first[n]
        vn[1] = vn_first[n]

        # position
        xn[1] = xn_first[n]

        # spacing
        sn[1] = xn1[1] - xn_first[n]

        # speed difference
        deltav[1] = vn_first[n] - vn1[1]


        ###### Gipps Calculations ############################
        
        for t in 2:(time_length-1)

            ## free flow
            vn_ff[t] = vn[t-1] + (2.5 * an * tau * (1 - (vn[t-1])/Vn)) * ((0.025 + (vn[t-1]/Vn))^(0.5))

            ## car following
            bcap_part_cf = (((vn1[t-1])^2)/bcap)

            vn_cf[t] = (bn_const * tau) + sqrt(((bn_const^2) * (tau^2)) - (bn_const * (2 * (xn1[t-1] - ln1 - xn[t-1]) - (vn[t-1] * tau) - bcap_part_cf)))

            ## gipps speed
            if (ismissing.(vn1[t-1]))
            
            vn[t] = vn_ff[t]
            
            else 
            
            vn[t] = min(vn_ff[t], vn_cf[t] )
            
            end


            ### if the speed is negative, make it zero
            vn[t] = ifelse(vn[t] < 0, 0, vn[t])
            
            
            ## acceleration
            bn[t-1] = (vn[t] - vn[t-1])/(resolution)
            
            
            
            ## position
            xn[t] = xn[t-1] + (vn[t-1] * resolution) + (0.5 * bn[t-1] * (resolution)^2)
            
            
            # spacing
            sn[t] = xn1[t] - xn[t] - ln1
            
            # speed difference
            deltav[t] = vn[t] - vn1[t]

        end

    # ################## Result in a dataframe ###################################
        result_dfn = DataFrame(fvn=n, Time =Time, xn1=xn1, vn1=vn1, ln1=ln1, bn=bn, xn=xn, vn=vn, sn=sn, deltav=deltav)
        
        
        list_of_N_veh[n] = result_dfn
        
        xn1 = xn
        vn1 = vn

    end

    result = reduce(vcat, list_of_N_veh)

    return result

end