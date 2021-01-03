from typing import Dict
from typing_extensions import Literal

from jax import jit, random
import jax.numpy as jnp

ParticleKeys = Literal["position", "direction", "energy"]

Particles = Dict[ParticleKeys, jnp.DeviceArray]


@jit
def a_jitable_pure_function_using_dict(rng_key, particles):
    num_particles = particles["position"].shape[-1]

    random_normal_numbers = random.normal(rng_key, shape=(7, num_particles))
    (rng_key,) = random.split(rng_key, 1)

    particles["position"] += random_normal_numbers[0:3, :]
    particles["direction"] += random_normal_numbers[3:6, :]
    particles["energy"] += random_normal_numbers[7, :]

    return rng_key, particles


def main():
    seed = 0
    num_particles = 1e6
    rng_key = random.PRNGKey(seed)

    particles: Particles = {
        "position": jnp.zeros((3, num_particles)),
        "direction": jnp.zeros((3, num_particles)),
        "energy": jnp.zeros((1, num_particles)),
    }

    rng_key, particles = a_jitable_pure_function_using_dict(rng_key, particles)
