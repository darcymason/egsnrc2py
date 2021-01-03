import time
from typing import Dict, Tuple
from typing_extensions import Literal

import matplotlib.pyplot as plt

from jax import jit, random
import jax.numpy as jnp

ParticleKeys = Literal["position", "direction", "energy"]

Particles = Dict[ParticleKeys, jnp.DeviceArray]


def random_walk(
    prng_key: jnp.DeviceArray, particles: Particles, iterations: int,
) -> Tuple[jnp.DeviceArray, Particles]:
    num_particles = particles["position"].shape[-1]

    for _ in range(iterations):
        random_normal_numbers = random.normal(prng_key, shape=(7, num_particles))
        (prng_key,) = random.split(prng_key, 1)

        particles["position"] += random_normal_numbers[0:3, :]
        particles["direction"] += random_normal_numbers[3:6, :]
        particles["energy"] += random_normal_numbers[7, :]

    return prng_key, particles


random_walk = jit(random_walk, static_argnums=(2,))


def timer(func):
    def wrap(*args, **kwargs):
        start = time.time()
        ret = func(*args, **kwargs)

        # See https://jax.readthedocs.io/en/latest/async_dispatch.html
        # for why this is needed.
        _, particles = ret
        for _, item in particles.items():
            item.block_until_ready()

        stop = time.time()
        duration = (stop - start) * 1000.0
        print("{:s} duration: {:.3f} ms".format(func.__name__, duration))
        return ret

    return wrap


random_walk = timer(random_walk)


def particles_zeros(num_particles: int) -> Particles:
    particles: Particles = {
        "position": jnp.zeros((3, num_particles)),
        "direction": jnp.zeros((3, num_particles)),
        "energy": jnp.zeros((1, num_particles)),
    }

    return particles


def main():
    seed = 0
    prng_key = random.PRNGKey(seed)
    num_particles = int(1e6)
    iterations = 10
    runs = 10

    particles = particles_zeros(num_particles)

    for _ in range(runs):
        prng_key, particles = random_walk(prng_key, particles, iterations)

    plt.scatter(particles["position"][0, 0:1000], particles["position"][1, 0:1000])
    plt.show()


if __name__ == "__main__":
    main()
