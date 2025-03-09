# Define Parameters
n_t <- 40  # Number of cycles (each cycle = 3 months, total ~10 years)
n_s <- 3   # Number of states
n_c <- 1000 # Initial cohort size

# Define state names
v_state_names <- c("Excellent Outcome", "Moderate/Severe Disability", "Death")

# Transition Probability Matrices
m_P_reteplase <- matrix(c(0.795, 0.162, 0.043,  # From Excellent Outcome
                           0.000, 0.966, 0.034,  # From Moderate/Severe Disability
                           0.000, 0.000, 1.000), # From Death
                         byrow = TRUE, 
                         nrow = 3, ncol = 3, 
                         dimnames = list(from = v_state_names, to = v_state_names))

m_P_alteplase <- matrix(c(0.704, 0.263, 0.034,  # From Excellent Outcome
                           0.000, 0.966, 0.034,  # From Moderate/Severe Disability
                           0.000, 0.000, 1.000), # From Death
                         byrow = TRUE, 
                         nrow = 3, ncol = 3, 
                         dimnames = list(from = v_state_names, to = v_state_names))

# Initialize State Membership Array
state_membership_reteplase <- array(NA_real_, dim = c(n_t, n_s), 
                                    dimnames = list(cycle = 1:n_t, state = v_state_names))
state_membership_alteplase <- array(NA_real_, dim = c(n_t, n_s), 
                                    dimnames = list(cycle = 1:n_t, state = v_state_names))

# Initial State Distribution
state_membership_reteplase[1, ] <- c(n_c, 0, 0)
state_membership_alteplase[1, ] <- c(n_c, 0, 0)

# Run Markov Model
for (i in 2:n_t) {
  state_membership_reteplase[i, ] <- state_membership_reteplase[i-1, ] %*% m_P_reteplase
  state_membership_alteplase[i, ] <- state_membership_alteplase[i-1, ] %*% m_P_alteplase
}

# Define Payoffs (Costs & QALYs per State)
m_payoffs <- matrix(c(10000, 25000, 0,  # Cost per state
                       0.9, 0.6, 0),     # QALY per state
                     nrow = 3, 
                     ncol = 2, 
                     byrow = FALSE, 
                     dimnames = list(state = v_state_names, 
                                     payoff = c("Cost", "QALY")))

# Calculate Payoff Traces
payoff_reteplase <- state_membership_reteplase %*% m_payoffs
payoff_alteplase <- state_membership_alteplase %*% m_payoffs

# Print Results
print("State Membership Over Time (Reteplase)")
print(state_membership_reteplase)

print("State Membership Over Time (Alteplase)")
print(state_membership_alteplase)

print("Total Costs and QALYs (Reteplase)")
print(payoff_reteplase)

print("Total Costs and QALYs (Alteplase)")
print(payoff_alteplase)
